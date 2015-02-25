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

import Language.CoreErlang.Syntax
import Language.CoreErlang.Pretty
import Parser.ErrM
import AST.AST

-- |The 'compileModule' function compiles a ModuleAST
--  to a CoreErlang.Syntax.Module
compileModule :: ModuleAST (Maybe TypeAST) -> Module
compileModule cMod@(ModuleAST mId exports defs) = Module (Atom mId) es attribs ds
  where attribs = []
        es      = compileExports exports cMod
        ds      = map compileFun defs ++ generateModuleInfo mId

-- |The 'compileModuleString' function compiles a ModuleAST
--  to a Core Erlang code string
compileModuleString :: ModuleAST (Maybe TypeAST) -> Err String
compileModuleString m = Ok $ prettyPrint cesModule
  where cesModule = compileModule m

-- |The 'compileExports' function compiles a list of ExportAST
--  to a list of CoreErlang.Syntax.Function
compileExports :: [ExportAST] -> ModuleAST (Maybe TypeAST) -> [Function]
compileExports es m = map (compileExport m) es ++ [mi0,mi1]
  where mi0  = Function (name,0)
        mi1  = Function (name,1)
        name = Atom "module_info"

-- |The 'compileExport' function compiles an ExportAST
--  to a CoreErlang.Syntax.Function
compileExport :: ModuleAST (Maybe TypeAST) -> ExportAST -> Function
compileExport m (ExportAST eId) = Function (Atom eId, getArity eId m)

-- |The 'compileFun' function compiles a DefAST
--  to a CoreErlang.FunDef
compileFun :: DefAST (Maybe TypeAST) -> FunDef
compileFun (DefAST fId t ast) = 
  FunDef (Constr (Function (Atom fId, typeToArity t))) (Constr (compileAST ast' []))
  where ast' = case ast of
                 (LamAST _ _) -> ast
                 _            -> LamAST [] ast
-- The empty lists here will be changed when we deal with parameters

-- |The 'compileType' function compiles a TypeAST
--  to CoreErlang data
--  Not implemented
compileType :: TypeAST -> String
compileType (VarType _vId)     = undefined
compileType (ConType _cId _ts) = undefined

-- |The 'compileExp' function compiles an AST
--  to a CoreErlang.Exp
--  Named and AppAst are implemented with parameterless functions in mind
--  AppAST will rely on that the first AST in the first occurence of
--  an AppAST will be a function identifier
compileAST :: AST -> [PatAST] -> Exp
compileAST (Named nId)     s = 
  if isIdBound nId s
    then Var $ compileLambdaPat (VarPat nId)
    else App (Exp (Constr (Fun (Function (Atom nId, 0))))) [] -- MIGHT NOT ALWAYS BE A FUNCTION, THINK ABOUT HOW TO DEAL WITH THIS
compileAST (LitStr s)      _ = Lit (LString s)
compileAST (LitInteger i)  _ = Lit (LInt i)
compileAST (LitDouble d)   _ = Lit (LFloat d) -- No double constructor in CoreErlang
compileAST (LitChar c)     _ = Lit (LChar c)
compileAST (LamAST pats a) s = Lambda (map compileLambdaPat pats) (Exp (Constr (compileAST a (pats++s))))
compileAST (AppAST a1 a2)  _ = App (Exp (Constr (Fun (Function (Atom nId, arity))))) args
  where arity       = toInteger $ length args
        args        = compileAppArgs a2
        (Named nId) = a1

-- |The 'compileAppArgs' function compiles a chain
--  of AST's in the form of AppAST to a list of expressions
compileAppArgs :: AST -> [Exps]
compileAppArgs a@(AppAST (Named _) _) = [Exp (Constr (compileAST a []))]
compileAppArgs (AppAST a1 a2)         = ann a1 ++ ann a2
  where ann x = case x of
                  (AppAST _ _)            -> compileAppArgs x
                  _                       -> [Exp (Constr (compileAST x []))]
compileAppArgs ast = [Exp (Constr (compileAST ast []))]

-- |The 'compileLambdaPat' function converts a
--  PatAST to a CoreErlang.Var
compileLambdaPat :: PatAST -> Var
compileLambdaPat (VarPat vId) = '_':vId -- _ garantuees valid core erlang variable name
compileLambdaPat WildPat     = "_"

-- |The 'isIdBound' checks if the given id is
--  bound in the given scope
isIdBound :: String -> [PatAST] -> Bool
isIdBound _ [] = False
isIdBound i (VarPat i':pats)
  | i == i' = True
  | otherwise  = isIdBound i pats
isIdBound i (_:pats) = isIdBound i pats

-- |The 'getArity' function gets the arity of the function
--  with the given id in the given ModuleAST
getArity :: String -> ModuleAST (Maybe TypeAST) -> Integer
getArity fId m = typeToArity $ getTypeSig fId m

-- |The 'typeToArity' returns the corresponding arity
--  of the given Maybe TypeAST. Nothing will just return 0
typeToArity :: Maybe TypeAST -> Integer
typeToArity t = case t of
                  Just (ConType _ []) -> 0 -- Parameterless functions
                  Just (ConType _ ts) -> toInteger $ length ts - 1
                  Just (VarType _)    -> undefined
                  Nothing             -> undefined

-- |The 'getTypeSig' function gets the Maybe TypeAST signature
--  of the function with the given id in the given ModuleAST
getTypeSig :: String -> ModuleAST (Maybe TypeAST) -> Maybe TypeAST
getTypeSig _ (ModuleAST _ _ []) = undefined -- Signature could not be found, code generator should not be invoked if this is the case
getTypeSig fId (ModuleAST mId es (def:defs))
  | fId == funId = typeSig
  | otherwise    = getTypeSig fId (ModuleAST mId es defs)
  where (DefAST funId typeSig _) = def

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
