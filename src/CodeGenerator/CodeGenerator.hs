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

import Language.CoreErlang.Syntax as CES
import AbsHopper
import Data.List

-- |The 'compileModule' function compiles a Module
--  to CoreErlang data
compileModule :: Module -> CES.Module
compileModule mod = CES.Module (Atom id) attribs ds
  where es      = compileExports exports mod
        ds      = map compileDef defs
        attribs = [] -- Not using attributes now
        (MModule (IdCon id) exports defs) = mod

compileExports :: [Export] -> Module -> [CES.Function]
compileExports es mod = mi0:(mi1:(map compileExport es mod))
  where mi0  = CES.Function (name,0)
        mi1  = CES.Function (name,1)
        name = CES.Atom "module_info"

-- |The 'compileExport' function compiles an Export
--  to CoreErlang data
compileExport :: Export -> Module -> CES.Function
compileExport (MExport (IdVar id)) = CES.Function (CES.Atom id, getArity id mod)

-- |The 'compileDef' function compiles a Def
--  to CoreErlang data
compileDef :: Def -> FunDef
compileDef (DFun (IdVar id) exp)       = undefined
compileDef (DCollected IdVar sig funs) = FunDef (compileSig sig) 

compileSig :: Def -> Function
compileSig (DSig (IdVar id) types) = Function (Atom id, length types - 1)

-- |The 'compileType' function compiles a Type
--  to CoreErlang data
compileType :: Type -> String
compileType (TName (IdCon id)) = undefined
compileType (TVar  (IdVar id)) = undefined
compileType TInt               = undefined

-- |The 'compileExp' function compiles an Exp
--  to CoreErlang data
--  EInfix should not be dealt with, the updated data structure
--  won't contain infix, will remove when implemented
compileExp :: Exp -> CES.Exp
compileExp (EVar (IdVar id))         = CES.EVar id
compileExp (ECon (IdCon id))         = Lit (Latom (Atom id))
compileExp (EOpr (IdOpr id))         = Op (Atom id) [] -- Not sure about this
compileExp (EString s)               = Lit (LString s)
compileExp (EChar c)                 = Lit (LChar c)
compileExp (EInteger i)              = Lit (LInt i)
compileExp (EDouble d)               = Lit (LFloat d) -- No double constructor in CoreErlang
compileExp (EInfix e1 (IdOpr id) e2) = undefined
compileExp (EApp e1 e2)              = App 
compileExp (ELambda pats e)          = Lambda (map compileLambdaPat pats) (CES.Exp (compileExp e))

-- |The 'compileLambdaPat' function converts a
--  lambda pattern to a CoreErlang var
compileLambdaPat :: Pat -> CES.Var
compileLambdaPat (_ (_ id)) = CES.Var id
compileLambdaPat PWild      = "_"

-- |The 'compilePat' function compiles a Pat
--  to CoreErlang data
compilePat :: Pat -> CES.Pat
compilePat (PCon (IdCon id)) = PLit (LAtom (Atom id)) 
compilePat (PVar (IdVar id)) = CES.PVar id
compilePat PWild             = CES.PVar "_"

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
--  Takes a String Function Id and Module as argument
getArity :: String -> Module -> Integer
getArity id mod = length types - 1
  where (DSig _ types) = getSig id mod

-- |The 'getSig' function returns the DSig with
--  the corresponding id
--  Takes a String Function Id and Module as argument
--  This function assumes that the given Module contains
--  a function signature with the given Function Id
getSig :: String -> Module -> Def
getSig id (MModule mId es (def:defs))
  | id == funId = def
  | otherwise   = findSig id (MModule mId es defs)
  where (DSig (IdVar funId) _) = def
