{-| 
Module      : CodeGenerator
Description : Generates CoreErlang
Copyright   : -
License     : -
Status      : Partly implemented. As of now this module works with the
              datatypes described in AbsGrammar.hs. This module should
              be translated to work with a processed version of these
              datatypes with (but not limited to) unitized annotated
              function definitions. This module expects every function to
              have a signature.

Module that generates CoreErlang code from an 
abstract module as seen in AbsGrammar.hs

This module is part of the HPR.language project
-}

module CodeGenerator.CodeGenerator where

import Language.CoreErlang.Syntax as CES
import Language.CoreErlang.Pretty as CEP
import Parser.ErrM
import Data.List
import AST.AST

-- |The 'compileModule' function compiles a ModuleAST
--  to a CoreErlang.Syntax.Module
compileModule :: ModuleAST (Maybe TypeAST) -> CES.Module
compileModule mod = CES.Module (Atom id) es attribs ds
  where es      = compileExports exports mod
        ds      = (map (compileFun mod) defs) ++ generateModuleInfo id
        attribs = [] -- Not using attributes now
        (ModuleAST id exports defs) = mod

-- |The 'compileModuleString' function compiles a ModuleAST
--  to a Core Erlang code string
compileModuleString :: ModuleAST (Maybe TypeAST) -> Err String
compileModuleString mod = Ok $ CEP.prettyPrint cesModule
  where cesModule = compileModule mod

-- |The 'compileExports' function compiles a list of ExportAST
--  to a list of CoreErlang.Syntax.Function
compileExports :: [ExportAST] -> ModuleAST (Maybe TypeAST) -> [CES.Function]
compileExports es mod = (map (compileExport mod) es) ++ [mi0,mi1]
  where mi0  = CES.Function (name,0)
        mi1  = CES.Function (name,1)
        name = CES.Atom "module_info"

-- |The 'compileExport' function compiles an ExportAST
--  to a CoreErlang.Syntax.Function
compileExport :: ModuleAST (Maybe TypeAST) -> ExportAST -> CES.Function
compileExport mod (ExportAST id) = CES.Function (CES.Atom id, getArity id mod)

-- |The 'compileFun' function compiles a DefAST
--  to a CoreErlang.FunDef
compileFun :: ModuleAST (Maybe TypeAST) -> DefAST (Maybe TypeAST) -> CES.FunDef
compileFun mod (DefAST id t ast) = 
  FunDef (Constr (Function (CES.Atom id, typeToArity t))) (Constr (compileAST (LamAST [] ast)))

-- |The 'compileType' function compiles a TypeAST
--  to CoreErlang data
--  Not implemented
compileType :: TypeAST -> String
compileType (VarType id)    = undefined
compileType (ConType id ts) = undefined

-- |The 'compileExp' function compiles an AST
--  to a CoreErlang.Exp
--  Named and AppAst are implemented with parameterless functions in mind
--  AppAST will rely on that the first AST in the first occurence of
--  an AppAST will be a function identifier
compileAST :: AST -> Exp
compileAST (Named id)      = App (Exp (Constr (Fun (Function (Atom id, 0))))) []
compileAST (LitStr s)      = Lit (LString s)
compileAST (LitInteger i)  = Lit (LInt i)
compileAST (LitDouble d)   = Lit (LFloat d) -- No double constructor in CoreErlang
compileAST (LitChar c)     = Lit (LChar c)
compileAST (LamAST pats a) = Lambda (map compileLambdaPat pats) (CES.Exp (Constr (compileAST a)))
compileAST (AppAST a1 a2)  = App (Exp (Constr (Fun (Function (Atom id, arity))))) args
  where arity      = toInteger $ length args
        args       = compileAppArgs a2
        (Named id) = a1

compileAppArgs :: AST -> [Exps]
compileAppArss a@(AppAST (Named id') a2) = App (Exp (Constr (compileAST a)))
compileAppArgs (AppAST a1 a2)            = ann a1 ++ ann a2
  where ann x = case x of
                  (AppAST _ _)            -> compileAppArgs x
                  _                       -> [Exp (Constr (compileAST x))]

-- |The 'compileLambdaPat' function converts a
--  PatAST to a CoreErlang.Var
compileLambdaPat :: PatAST -> Var
compileLambdaPat (VarPat id) = id
compileLambdaPat WildPat     = "_"

-- |The 'getArity' function gets the arity of the function
--  with the given id in the given ModuleAST
getArity :: String -> ModuleAST (Maybe TypeAST) -> Integer
getArity id mod = typeToArity $ getTypeSig id mod

-- |The 'typeToArity' returns the corresponding arity
--  of the given Maybe TypeAST. Nothing will just return 0
typeToArity :: Maybe TypeAST -> Integer
typeToArity t = case t of
                  Just (ConType _ ts) -> toInteger $ length ts
                  Nothing             -> 0

-- |The 'getTypeSig' function gets the Maybe TypeAST signature
--  of the function with the given id in the given ModuleAST
getTypeSig :: String -> ModuleAST (Maybe TypeAST) -> Maybe TypeAST
getTypeSig id (ModuleAST mId es (def:defs))
  | id == funId = typeSig
  | otherwise   = getTypeSig id (ModuleAST mId es defs)
  where (DefAST funId typeSig _) = def

-- |The 'generateModuleInfo' function generates a list of
--  CoreErlang.FunDec of containing the module_info/0 and
--  module_info/1 functions
--  Takes a String module id as argument
generateModuleInfo :: String -> [FunDef]
generateModuleInfo id = [mi0,mi1]
  where mi0 = FunDef (Constr (Function (Atom "module_info",0)))
                     (Constr (Lambda [] (Exp (Constr (ModCall
                        (Exp (Constr (Lit (LAtom (Atom "erlang")))),
                         Exp (Constr (Lit (LAtom (Atom "get_module_info")))))
                        [Exp (Constr (Lit (LAtom (Atom id))))])))))
        mi1 = FunDef (Constr (Function (Atom "module_info",1)))
                     (Constr (Lambda ["_cor0"] (Exp (Constr (ModCall
                        (Exp (Constr (Lit (LAtom (Atom "erlang")))),
                         Exp (Constr (Lit (LAtom (Atom "get_module_info")))))
                        [Exp (Constr (Lit (LAtom (Atom id)))),Exp (Constr (Var "_cor0"))])))))
