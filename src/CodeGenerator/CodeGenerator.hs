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

-- |The 'compileModule' function compiles a hopper Module
--  to a CoreErlang<F6>fe<F6>tax.Module
compileModule :: HPR.Module Type -> CES.Module
compileModule m@(Mod mId exports defs datas) = CES.Module (Atom mId) es as ds
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
  CES.FunDef (Constr (Function (Atom fId, typeToArity t))) (Constr (compileExp e'))
  where e' = case e of
               (ELambda _ _) -> e
               _             -> (ELambda [] e)

-- |The 'compileExp' function compiles an AST
--  to a CoreErlang.Exp
--  Named and AppAst are implemented with parameterless functions in mind
--  AppAST will rely on that the first AST in the first occurence of
--  an AppAST will be a function identifier
compileExp :: Expression -> CES.Exp
compileExp (EVar nId)        = Var $ compileLambdaPat (HPR.PVar nId)
compileExp (ECon c)          = Lit $ LAtom $ Atom c
compileExp (ELit l)          = Lit $ compileLiteral l
compileExp (ETuple es)       = Tuple $ map (\e -> Exp (Constr (compileExp e))) es
compileExp (ELambda pats e)  = Lambda (map compileLambdaPat pats) (Exp (Constr (compileExp e)))
compileExp e@(EApp _ _)      = error $ "Unexpected EApp in code gen: " ++ show e
compileExp (EVal i args)     = App f a
  where f = Exp (Constr (CES.Fun (Function (Atom i, toInteger (length args)))))
        a = map (\e -> Exp (Constr (compileExp e))) args
compileExp (ECase e cases)   = Case (Exp (Constr (compileExp e))) (compileCases cases)
compileExp (ECall mId fId e) = ModCall (m, fun) (map f as)
  where m           = Exp (Constr (Lit (LAtom (Atom mId))))
        fun         = Exp (Constr (Lit (LAtom (Atom fId))))
        (ETuple as) = e
        f x = Exp (Constr (compileExp x))

-- |The 'compileLambdaPat' function converts a
--  PatAST to a CoreErlang.Var
--
--  We need to think about how to deal with tuples in lambdas.
--  In hopper, the lambda patterns may be variables, wildcards or
--  tuples of variables. In core erlang the lambda patterns is just
--  a list of variables. In this case, a tuple would be translated
--  to a single variable, which is then matched in a generated case
--  clause within the lambda expression.
compileLambdaPat :: Pattern -> Var
compileLambdaPat PWild           = "_"
compileLambdaPat (HPR.PTuple ps) = error $ "Tuple not implemented yet, got: " ++ show ps
compileLambdaPat (HPR.PLit l)    = error $ "Unallowed literal in lambda pattern: " ++ show l 
compileLambdaPat (HPR.PVar v)    = '_':v
compileLambdaPat (PCon c pts)    = error $ "Constructors not implemented: " ++ show c

-- |The 'compileCases' function converts a list of
--  cases to a list of annotated alts as seen in
--  Language.CoreErlang.Syntax case expressions
compileCases :: [(Pattern, Expression)] -> [Ann Alt]
compileCases [] = []
compileCases ((p, e):rest) = Constr (Alt pats guard exps ) : compileCases rest
  where pats  = Pat $ compileCasePat p
        guard = Guard (Exp (Constr (Lit (LAtom (Atom "true"))))) -- Change when guards are implemented
        exps  = Exp (Constr (compileExp e))

-- |The 'getCasePatterns' function converts a pattern
--  to a list of patterns. This means a tuple will be
--  converted to a list of its patterns.
getCasePatterns :: Pattern -> [Pattern]
getCasePatterns (HPR.PTuple ps) = ps
getCasePatterns p               = [p]

-- |The 'compileCasePat' function compiles a hopper pattern
--  to a core erlang pattern
compileCasePat :: Pattern -> Pat
compileCasePat p@(HPR.PVar _)    = CES.PVar $ compileLambdaPat p
compileCasePat (HPR.PCon c pts)  = CES.PTuple $ (CES.PLit $ LAtom $ Atom c) : map compileCasePat pts
compileCasePat p@PWild           = CES.PVar $ compileLambdaPat p
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
getTypeSig fId (Mod _ _ [] _) = error $ "Could not find function when looking for signature: " ++ fId
getTypeSig fId (Mod mId es (HPR.Fun funId typeSig _:defs) datas)
  | fId == funId = typeSig
  | otherwise    = getTypeSig fId (Mod mId es defs datas)

-- |The 'gitSignatures' function gets the function
--  signatures from the give module
getSignatures :: HPR.Module Type -> [(Identifier, Integer)]
getSignatures (Mod _ _ defs _) = map (\(HPR.Fun i sig _) -> (i, 1)) defs

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
