{-| 
Module      : CodeGenerator
Description : Generates CoreErlang
Copyright   : -
License     : -
Status      : Partly implemented. As of now this module works with the
              datatypes described in AbsGrammar.hs. This module should
              be translated to work with a processed version of these
              datatypes with (but not limited to) unitized annotated
              function definitions

Module that generates CoreErlang code from an 
abstract module as seen in AbsGrammar.hs

This module is part of the Hopper language project
-}

module CodeGenerator where

-- import CoreErlang.Syntax
import AbsGrammar
import Data.List

-- |The 'compileModule' function compiles a Module
--  to CoreErlang code
compileModule :: Module -> String
compileModule (MModule (IdCon id) exports defs) = 
  intercalate " " ["module", "\'" ++ id ++ "\'", es, attribs, ds, "end"]
  where es      = compileExports exports
        ds      = compileDefs defs
        attribs = "attributes []" -- Not using attributes now

-- |The 'compileExports' function compiles a list of Exports
--  to CoreErlang code
compileExports :: [Export] -> String
compileExports es = "[" ++ intercalate "," (map compileExport es) ++ "]"

-- |The 'compileExport' function compiles an Export
--  to CoreErlang code
compileExport :: Export -> String
compileExport (MExport (IdVar id)) = "\'" ++ id ++ "\'/" ++ getArity id

-- |The 'compileDefs' function compiles a list of Defs
--  to CoreErlang code
compileDefs :: [Def] -> String
compileDefs defs = intercalate " " $ map compileDef defs

-- |The 'compileDef' function compiles a Def
--  to CoreErlang code
compileDef :: Def -> String
compileDef (DSig (IdVar id) types) = undefined
compileDef (DFun (IdVar id) exp)   = undefined

-- |The 'compileType' function compiles a Type
--  to CoreErlang code
compileType :: Type -> String
compileType (TName (IdCon id)) = undefined
compileType (TVar  (IdVar id)) = undefined
compileType TInt               = undefined

-- |The 'compileExp' function compiles an Exp
--  to CoreErlang code
compileExp :: Exp -> String
compileExp (EVar (IdVar id))         = undefined
compileExp (ECon (IdCon id))         = undefined
compileExp (EOpr (IdOpr id))         = undefined
compileExp (EString s)               = undefined
compileExp (EChar c)                 = undefined
compileExp (EInteger i)              = undefined
compileExp (EDouble d)               = undefined
compileExp (EInfix e1 (IdOpr id) e2) = undefined
compileExp (EApp e1 e2)              = undefined
compileExp (ELambda pats e)          = undefined

-- |The 'compilePat' function compiles a Pat
--  to CoreErlang code
compilePat :: Pat -> String
compilePat (PCon (IdCon id)) = undefined
compilePat (PVar (IdVar id)) = undefined
compilePat PWild             = undefined

-- |The 'generateModuleInfo' function generates the
--  CoreErlang code for the module info functions
--  Takes a String Module Id as argument
generateModuleInfo :: String -> String
generateModuleInfo id = 
  "\'module_info\'/0 = fun () -> call \'erlang\':" ++
  "\'get_module_info\' (\'" ++ id ++ "\')" ++
  "\'module_info\'/1 = fun (_cor0) -> call \'erlang\':" ++
  "\'get_module_info\'(\'" ++ id ++ "\', _cor0)"


-- |The 'getArity' function returns the arity of
--  the function with the given id
--  Takes a String Function Id as argument
getArity :: String -> String
getArity id = undefined
