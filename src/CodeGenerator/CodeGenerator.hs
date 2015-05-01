{-| 
Module      : CodeGenerator
Description : Generates CoreErlang
Copyright   : -
License     : -
Status      : Partly implemented. As of now this module works with the
              datatypes described in AST.hs. This module should
              be translated to work with a processed version of these
              datatypes with (but not limited to) unitized annotated
              function definitions. This module expects every function to
              have a signature.

Module that generates CoreErlang code from an 
abstract module as seen in AST.hs

This module is part of the Hopper language project
-}

module CodeGenerator.CodeGenerator where

import Language.CoreErlang.Syntax as CES
import Language.CoreErlang.Pretty
import Utils.ErrM
import AST.AST as HPR

import Data.Char
import qualified Data.Set as S
import Data.List (break, intercalate)
import qualified Control.Monad.Reader as R

-- |The 'compileModule' function compiles a hopper Module
--  to a CoreErlang<F6>fe<F6>tax.Module
compileModule :: HPR.Module Type -> CES.Module
compileModule m@(Mod mId exports _imports defs datas) = CES.Module (Atom mId) es as ds
  where as = []
        ds = map compileFun defs ++ generateModuleInfo mId
        es = compileExports exports m

-- |The 'compileModuleString' function compiles a hoppper
--  to a Core Erlang code string
compileModuleString :: HPR.Module Type -> Err String
compileModuleString m = Ok $ prettyPrint cesModule
  where cesModule = compileModule m

-- |The 'compileExports' function compiles a list of Identifier
--  to a list of CoreErlang.Syntax.Function
compileExports :: [Identifier] -> HPR.Module Type -> [CES.Function]
compileExports es m = map (compileExport m) es ++ [mi0,mi1]
  where mi0  = CES.Function (name,0)
        mi1  = CES.Function (name,1)
        name = Atom "module_info"

-- |The 'compileExport' function compiles an Identifier
--  to a CoreErlang.Syntax.Function
compileExport :: HPR.Module Type -> Identifier -> CES.Function
compileExport m eId = CES.Function (Atom eId, getArity eId m)

-- |The 'compileFun' function compiles a hopper Function
--  to a CoreErlang.FunDef
compileFun :: HPR.Function Type -> FunDef
compileFun (HPR.Fun fId t e) =
  CES.FunDef (Constr (Function (Atom fId, typeToArity t))) (Constr (R.runReader (compileExp e') S.empty))
  where e' = case e of
               (ELambda _ _) -> e
               _             -> (ELambda [] e)

-- |The 'compileExp' function compiles an AST
--  to a CoreErlang.Exp
--  Named and AppAst are implemented with parameterless functions in mind
--  AppAST will rely on that the first AST in the first occurence of
--  an AppAST will be a function identifier
compileExp :: Expression -> R.Reader (S.Set Identifier) CES.Exp
compileExp (EVar nId)         = case strToName nId of
                                  (Nothing, _) -> do isVariable <- R.asks $ S.member nId
                                                     return $ if isVariable
                                                                 then Var nId
                                                                 else CES.Fun $ __fun nId
                                  (Just mId, fId) -> return $ modCall 
                                                     (atom "erlang")
                                                     (atom "make_fun")
                                                     [atom $ intercalate "." mId,
                                                     atom $ "__"++fId,
                                                     Lit $ LInt 0]
compileExp (ELit l)           = return $ Lit $ compileLiteral l
compileExp (ETuple es)        = fmap Tuple $ mapM (\e -> fmap exps $ compileExp e) es
compileExp l@(ELambda pats e) = compileLambda l
compileExp (EApp ef ex)       = do ef' <- compileExp ef
                                   ex' <- compileExp ex
                                   return $ App (exps ef') [exps ex']
compileExp (ECase e cases)    = do exp <- compileExp e
                                   alts <- mapM (\(pat,res)->
                                                 do 
                                                   let cpat = compileCasePat pat
                                                       vars = namesOccP pat
                                                   cres <- R.local 
                                                           (inserts vars) $
                                                        compileExp res
                                                   return $ Constr $ Alt
                                                       (pats cpat)
                                                       (Guard$
                                                        exps$
                                                        atom"true")
                                                       (exps cres)) cases
                                   return $ Case (exps exp) alts
compileExp (ECall mId fId e)  = fmap (ModCall (m, fun)) (mapM (fmap exps . compileExp) as)
  where m           = Exp (Constr (Lit (LAtom (Atom mId))))
        fun         = Exp (Constr (Lit (LAtom (Atom fId))))
        (ETuple as) = e
compileExp e                  = error $ "Illegal expression: " ++ show e

pats :: Pat -> Pats
pats = Pat

exps :: Exp -> Exps
exps = Exp . Constr

atom = Lit . LAtom . Atom 

namesOccP pat = case pat of
                  HPR.PVar pId    -> [pId]
                  HPR.PCon _ pats -> pats >>= namesOccP
                  HPR.PTuple pats -> pats >>= namesOccP
                  _               -> []

-- TODO WRITE TYPE SIG
inserts ns set = foldr (\n set -> S.insert n set) set ns

-- |The 'compileLambda' function converts a
--  hopper lambda expression to a core erlang lambda.
--
-- The main issue here is that the core erlang lambdas only
-- have a list of variables as the pattern. Whenever the hopper
-- pattern contains something else, it should replace it with a
-- fresh variable and wrap the expression with a case clause
compileLambda :: Expression -> R.Reader (S.Set Identifier) CES.Exp
compileLambda l@(ELambda pats e) = compileExp $ lambdaToCase l
compileLambda e                  = error $ "Not a lambda: " ++ show e

-- \a (b,c) -> a + b
-- \X@1 X@0 -> case (X@1, X@0) of
--               (a, (b,c)) -> a + b
lambdaToCase :: Expression -> Expression
lambdaToCase (ELambda pats e) = let vns = map (("X@"++) . show) (zipWith const [1..] pats) in
       ELambda (map HPR.PVar vns) $ ECase (HPR.ETuple $ map EVar vns) [(HPR.PTuple pats, e)]

--  to a list of patterns. This means a tuple will be
--  converted to a list of its patterns.
getCasePatterns :: Pattern -> [Pattern]
getCasePatterns (HPR.PTuple ps) = ps
getCasePatterns p               = [p]

-- |The 'compileCasePat' function compiles a hopper pattern
--  to a core erlang pattern
compileCasePat :: Pattern -> Pat
compileCasePat (HPR.PVar i)     = CES.PVar i
compileCasePat (HPR.PCon c pts) = CES.PTuple $ (CES.PLit $ LAtom $ Atom c') : map compileCasePat pts
  where c' = case c of
               ':':xs -> xs
               x:xs   -> toLower x : xs
compileCasePat PWild             = CES.PVar "_" 
compileCasePat (HPR.PLit l)      = CES.PLit $ compileLiteral l
compileCasePat (HPR.PTuple pats) = CES.PTuple $ map compileCasePat pats

-- |The 'compileLiteral' function compiles a hopper literal
--  to a core erlang literal
compileLiteral :: HPR.Literal -> CES.Literal
compileLiteral (LS s) = LString s
compileLiteral (LC c) = LChar c
compileLiteral (LI i) = LInt i
compileLiteral (LD d) = LFloat d -- No double constructor in CoreErlang

-- |The 'isIdBound' function checks if the given id is
--  bound in the given scope
isIdBound :: String -> [Pattern] -> Bool
isIdBound _ [] = False
isIdBound i (HPR.PVar i':pats)
  | i == i' = True
  | otherwise  = isIdBound i pats
isIdBound i (_:pats) = isIdBound i pats

-- |The 'getArity' function gets the arity of the function
--  with the given id in the given ModuleAST
getArity :: String -> HPR.Module Type -> Integer
getArity fId m = typeToArity $ getTypeSig fId m

-- |The 'typeToArity' returns the corresponding arity
--  of the given Signature. Nothing will just return 0
typeToArity :: Type -> Integer
typeToArity _ = 1 -- TODO NOT CORRECT AT ALL

-- |The 'getTypeSig' function gets the Signature
--  of the function with the given id in the given ModuleAST
getTypeSig :: String -> HPR.Module Type -> Type
getTypeSig fId (Mod _ _ _ [] _) = error $ "Could not find function when looking for signature: " ++ fId
getTypeSig fId (Mod mId es is (HPR.Fun funId typeSig _:defs) datas)
  | fId == funId = typeSig
  | otherwise    = getTypeSig fId (Mod mId es is defs datas)

-- |The 'gitSignatures' function gets the function
--  signatures from the give module
getSignatures :: HPR.Module Type -> [(Identifier, Integer)]
getSignatures (Mod _ _ _ defs _) = map (\(HPR.Fun i sig _) -> (i, 1)) defs

__fun :: Identifier -> CES.Function
__fun s = Function (Atom$"__"++s,0)

modCall :: Exp -> Exp -> [Exp] -> Exp
modCall mod fn args = ModCall (exps mod,exps fn) $ map exps args

strToName :: Identifier -> (Maybe [String], String)
strToName (c:s) | isUpper c = case break (=='.') (c:s) of
                                (con,"") -> (Nothing, con)
                                (mod,'.':s') -> addModule mod $ strToName s'
                | otherwise = (Nothing, c:s )
                where
                  addModule m (Nothing, s) = (Just[m], s)
                  addModule m (Just ms, s) = (Just$m:ms, s)

-- |The 'generateModuleInfo' function generates a list of
--  CoreErlang.FunDec of containing the module_info/0 and
--  module_info/1 functions
--  Takes a String module id as argument
generateModuleInfo :: String -> [FunDef]
generateModuleInfo mId = [mi0,mi1]
  where mi0 = FunDef (Constr (Function (Atom "module_info",0)))
                     (Constr (Lambda [] (Exp (Constr (ModCall
                        (Exp (Constr (Lit (LAtom (Atom "erlang")))),
                         Exp (Constr (Lit (LAtom (Atom "get_module_info")))))
                        [Exp (Constr (Lit (LAtom (Atom mId))))])))))
        mi1 = FunDef (Constr (Function (Atom "module_info",1)))
                     (Constr (Lambda ["_cor0"] (Exp (Constr (ModCall
                        (Exp (Constr (Lit (LAtom (Atom "erlang")))),
                         Exp (Constr (Lit (LAtom (Atom "get_module_info")))))
                        [Exp (Constr (Lit (LAtom (Atom mId)))),Exp (Constr (Var "_cor0"))])))))
