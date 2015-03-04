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
compileModule :: HPR.Module Signature -> CES.Module
compileModule cMod@(Mod mId exports defs) = CES.Module (Atom mId) es as ds
  where as = []
        es = compileExports exports cMod
        ds = map compileFun defs ++ generateModuleInfo mId

-- |The 'compileModuleString' function compiles a hoppper
--  to a Core Erlang code string
compileModuleString :: HPR.Module Signature -> Err String
compileModuleString m = Ok $ prettyPrint cesModule
  where cesModule = compileModule m

-- |The 'compileExports' function compiles a list of Identifier
--  to a list of CoreErlang.Syntax.Function
compileExports :: [Identifier] -> HPR.Module Signature -> [CES.Function]
compileExports es m = map (compileExport m) es ++ [mi0,mi1]
  where mi0  = CES.Function (name,0)
        mi1  = CES.Function (name,1)
        name = Atom "module_info"

-- |The 'compileExport' function compiles an Identifier
--  to a CoreErlang.Syntax.Function
compileExport :: HPR.Module Signature -> Identifier -> CES.Function
compileExport m eId = CES.Function (Atom eId, getArity eId m)

-- |The 'compileFun' function compiles a hopper Function
--  to a CoreErlang.FunDef
compileFun :: HPR.Function Signature -> FunDef
compileFun (HPR.Fun fId t e) = 
  CES.FunDef (Constr (Function (Atom fId, typeToArity t))) (Constr (compileExp e' []))
  where e' = case e of
                 ELambda{} -> e
                 _         -> ELambda t [] e
-- The empty lists here will be changed when we deal with parameters

-- |The 'compileExp' function compiles an AST
--  to a CoreErlang.Exp
--  Named and AppAst are implemented with parameterless functions in mind
--  AppAST will rely on that the first AST in the first occurence of
--  an AppAST will be a function identifier
compileExp :: Expression Signature -> [Pattern] -> CES.Exp
compileExp (EVar _ nId) s = 
  if isIdBound nId s
    then Var $ compileLambdaPat (HPR.PVar nId)
    else App (Exp (Constr (CES.Fun (Function (Atom nId, 0))))) [] -- MIGHT NOT ALWAYS BE A FUNCTION, THINK ABOUT HOW TO DEAL WITH THIS
compileExp (ECon _ _)         _ = undefined -- when does this happen?
compileExp (ELit _ l)         _ = Lit $ compileLiteral l
compileExp (ETuple _ es)      s = Tuple $ map (\e -> Exp (Constr (compileExp e s))) es
compileExp (ELambda _ pats e) s = Lambda (map compileLambdaPat pats) 
                                         (Exp (Constr (compileExp e (pats++s))))
compileExp (EApp _ e1 e2)     _ = App (Exp (Constr (CES.Fun (CES.Function (Atom nId, arity))))) args
  where arity        = toInteger $ length args
        args         = compileAppArgs e2
        (EVar _ nId) = e1
compileExp (ECase _ e cases)  s = Case (Exp (Constr (compileExp e s))) (compileCases cases)

-- |The 'compileAppArgs' function compiles a chain
--  of AST's in the form of AppAST to a list of expressions
compileAppArgs :: Expression Signature -> [Exps]
compileAppArgs e@(EApp _ (EVar _ _) _) = [Exp (Constr (compileExp e []))]
compileAppArgs (EApp _ e1 e2)          = ann e1 ++ ann e2
  where ann x = case x of
                  EApp{} -> compileAppArgs x
                  _      -> [Exp (Constr (compileExp x []))]
compileAppArgs e = [Exp (Constr (compileExp e []))]

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
compileLambdaPat (HPR.PVar vId)  = '_':vId -- _ garantuees valid core erlang variable name
compileLambdaPat PWild           = "_"
compileLambdaPat (HPR.PTuple ps) = undefined -- TODO
compileLambdaPat _               = undefined

-- |The 'compileCases' function converts a list of
--  cases to a list of annotated alts as seen in
--  Language.CoreErlang.Syntax case expressions
compileCases :: [(Pattern, Expression Signature)] -> [Ann Alt]
compileCases [] = []
compileCases ((p, e):rest) = Constr (Alt pats guard exps ) : compileCases rest
  where pats  = Pat $ compileCasePat p
        guard = Guard (Exps (Constr []))
        exps  = Exp (Constr (compileExp e [])) -- TODO add virables in pats to scope

-- |The 'compileCasePat' function compiles a hopper pattern
--  to a core erlang pattern
compileCasePat :: Pattern -> Pat
compileCasePat p@(HPR.PVar _)    = CES.PVar $ compileLambdaPat p
compileCasePat p@PWild           = CES.PVar $ compileLambdaPat p
compileCasePat (HPR.PLit l)      = CES.PLit $ compileLiteral l
compileCasePat (HPR.PTuple pats) = CES.PTuple $ map compileCasePat pats
compileCasePat _                 = undefined

-- |The 'compileLiteral' function compiles a hopper literal
--  to a core erlang literal
compileLiteral :: HPR.Literal -> CES.Literal
compileLiteral (LS s) = LString s
compileLiteral (LC c) = LChar c
compileLiteral (LI i) = LInt i
compileLiteral (LD d) = LFloat d -- No double constructor in CoreErlang

-- |The 'isIdBound' checks if the given id is
--  bound in the given scope
isIdBound :: String -> [Pattern] -> Bool
isIdBound _ [] = False
isIdBound i (HPR.PVar i':pats)
  | i == i' = True
  | otherwise  = isIdBound i pats
isIdBound i (_:pats) = isIdBound i pats

-- |The 'getArity' function gets the arity of the function
--  with the given id in the given ModuleAST
getArity :: String -> HPR.Module Signature -> Integer
getArity fId m = typeToArity $ getTypeSig fId m

-- |The 'typeToArity' returns the corresponding arity
--  of the given Signature. Nothing will just return 0
typeToArity :: Signature -> Integer
typeToArity t = toInteger $ length t - 1

-- |The 'getTypeSig' function gets the SignatureAST signature
--  of the function with the given id in the given ModuleAST
getTypeSig :: String -> HPR.Module Signature -> Signature
getTypeSig _ (Mod _ _ []) = undefined -- Signature could not be found, code generator should not be invoked if this is the case
getTypeSig fId (Mod mId es (HPR.Fun funId typeSig _:defs))
  | fId == funId = typeSig
  | otherwise    = getTypeSig fId (Mod mId es defs)

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
