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

module CodeGenerator where

import Language.CoreErlang.Syntax as CES
import AbsHopper as HPR
import Data.List

-- |The 'compileModule' function compiles a AbsHPR.Module
--  to a CoreErlang.Syntax.Module
compileModule :: HPR.Module -> CES.Module
compileModule mod = CES.Module (Atom id) es attribs ds
  where es      = compileExports exports mod
        ds      = (map compileCollected defs) ++ generateModuleInfo id
        attribs = [] -- Not using attributes now
        (MModule (IdCon id) exports defs) = mod

compileExports :: [Export] -> HPR.Module -> [CES.Function]
compileExports es mod = (map (compileExport mod) es) ++ [mi0,mi1]
  where mi0  = CES.Function (name,0)
        mi1  = CES.Function (name,1)
        name = CES.Atom "module_info"

-- |The 'compileExport' function compiles an Export
--  to a CoreErlang.Syntax.Function
compileExport :: HPR.Module -> Export -> CES.Function
compileExport mod (MExport (IdVar id)) = CES.Function (CES.Atom id, getArity id mod)

-- |The 'compileDef' function compiles a Def
--  to a CoreErlang.FunDef
--  Not dealing with arguments yet, hence the empty list
compileCollected :: Def -> FunDef
compileCollected (DCollected (IdVar id) sig funs) = FunDef s f
  where s = Constr (compileSig sig)
        f = Constr (Lambda [] (CES.Exp (Constr (compileFun funs))))

-- |The 'compileSig' function compiles a Def DSig
--  to a CoreErlang.Function
compileSig :: Def -> Function
compileSig (DSig (IdVar id) types) = Function (Atom id, toInteger $ length types - 1)

-- |The 'compileFun' function compiles a list of Def DFun
--  to a CoreErlang.Exp
compileFun :: [Def] -> CES.Exp
compileFun [DFun (IdVar id) exp] = compileExp exp

-- |The 'compileType' function compiles a Type
--  to CoreErlang data
--  Not implemented
compileType :: Type -> String
compileType (TName (IdCon id)) = undefined
compileType (TVar  (IdVar id)) = undefined

-- |The 'compileExp' function compiles an Exp
--  to a CoreErlang.Exp
--  EInfix should not be dealt with, the updated data structure
--  won't contain infix, will remove when implemented
--  EApp not implemented, ELambda might need verification
compileExp :: HPR.Exp -> CES.Exp
compileExp (EVar (IdVar id))         = Var id
compileExp (ECon (IdCon id))         = Lit (LAtom (Atom id))
compileExp (EOpr (IdOpr id))         = Op (Atom id) [] -- Not sure about this
compileExp (EString s)               = Lit (LString s)
compileExp (EChar c)                 = Lit (LChar c)
compileExp (EInteger i)              = Lit (LInt i)
compileExp (EDouble d)               = Lit (LFloat d) -- No double constructor in CoreErlang
compileExp (EInfix e1 (IdOpr id) e2) = undefined
compileExp (EApp e1 e2)              = undefined 
compileExp (ELambda pats e)          = Lambda (map compileLambdaPat pats) (CES.Exp (Constr (compileExp e)))

-- |The 'compileLambdaPat' function converts a
--  lambda pattern to a CoreErlang.Var
compileLambdaPat :: HPR.Pat -> CES.Var
compileLambdaPat (PCon (IdCon id))        = id
compileLambdaPat (HPR.PVar (IdVar id)) = id
compileLambdaPat PWild                    = "_"

-- |The 'compilePat' function compiles a Pat
--  to a CoreErlang.Pat
compilePat :: HPR.Pat -> CES.Pat
compilePat (PCon (IdCon id))        = PLit (LAtom (Atom id)) 
compilePat (HPR.PVar (IdVar id)) = CES.PVar id
compilePat PWild                    = CES.PVar "_"

-- |The 'getArity' function returns the arity of
--  the function with the given id
--  Takes a String function id and the complete AbsHPR.Module as argument
--  Used when creating the export list
getArity :: String -> HPR.Module -> Integer
getArity id mod = toInteger $ length types - 1
  where (DSig _ types) = getSig id mod

-- |The 'getSig' function returns the DSig with
--  the corresponding id
--  Takes a String function id and the complete AbsHPR.Module as argument
--  This function assumes that the given Module contains
--  a function signature with the given Function Id
getSig :: String -> HPR.Module -> Def
getSig id (MModule mId es (def:defs))
  | id == funId = sig
  | otherwise   = getSig id (MModule mId es defs)
  where (DSig (IdVar funId) types) = sig
        (DCollected _ sig _) = def

-- |The 'generateModuleInfo' function generates a list of
--  CoreErlang.FunDec of containing the module_info/0 and
--  module_info/1 functions
--  Takes a String module id as argument
generateModuleInfo :: String -> [FunDef]
generateModuleInfo id = [mi0,mi1]
  where mi0 = FunDef (Constr (Function (Atom "module_info",0))) (Constr (Lambda [] (Exp (Constr (ModCall (Exp (Constr (Lit (LAtom (Atom "erlang")))),Exp (Constr (Lit (LAtom (Atom "get_module_info"))))) [Exp (Constr (Lit (LAtom (Atom id))))])))))
        mi1 = FunDef (Constr (Function (Atom "module_info",1))) (Constr (Lambda ["_cor0"] (Exp (Constr (ModCall (Exp (Constr (Lit (LAtom (Atom "erlang")))),Exp (Constr (Lit (LAtom (Atom "get_module_info"))))) [Exp (Constr (Lit (LAtom (Atom id)))),Exp (Constr (Var "_cor0"))])))))
