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
                 (ELambda _ _ _) -> e
                 _               -> ELambda t [] e
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
compileExp (ELit _ (LS s))    _ = Lit (LString s)
compileExp (ELit _ (LC c))    _ = Lit (LChar c)
compileExp (ELit _ (LI i))    _ = Lit (LInt i)
compileExp (ELit _ (LD d))    _ = Lit (LFloat d) -- No double constructor in CoreErlang
compileExp (ELambda _ pats e) s = Lambda (map compileLambdaPat pats) 
                                         (Exp (Constr (compileExp e (pats++s))))
compileExp (EApp _ e1 e2)     _ = App (Exp (Constr (CES.Fun (CES.Function (Atom nId, arity))))) args
  where arity        = toInteger $ length args
        args         = compileAppArgs e2
        (EVar _ nId) = e1
compileExp (ECase _) _ = undefined -- wat

-- |The 'compileAppArgs' function compiles a chain
--  of AST's in the form of AppAST to a list of expressions
compileAppArgs :: Expression Signature -> [Exps]
compileAppArgs e@(EApp _ (EVar _ _) _) = [Exp (Constr (compileExp e []))]
compileAppArgs (EApp _ e1 e2)          = ann e1 ++ ann e2
  where ann x = case x of
                  (EApp _ _ _) -> compileAppArgs x
                  _            -> [Exp (Constr (compileExp x []))]
compileAppArgs e = [Exp (Constr (compileExp e []))]

-- |The 'compileLambdaPat' function converts a
--  PatAST to a CoreErlang.Var
compileLambdaPat :: Pattern -> Var
compileLambdaPat (HPR.PVar vId) = '_':vId -- _ garantuees valid core erlang variable name
compileLambdaPat PWild          = "_"
compileLambdaPat (PCon _)       = undefined
compileLambdaPat (HPR.PLit _)   = undefined

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
getTypeSig fId (Mod mId es ((HPR.Fun funId typeSig _):defs))
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
