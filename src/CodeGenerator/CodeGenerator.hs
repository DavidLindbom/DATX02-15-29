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
import AST.AST
import Data.List

-- |The 'compileModule' function compiles a AbsHPR.Module
--  to a CoreErlang.Syntax.Module
compileModule :: AST.Module a -> CES.Module
compileModule mod = CES.Module (Atom id) es attribs ds
  where es      = compileExports exports mod
        ds      = (map (comileFun mod) defs) ++ generateModuleInfo id
        attribs = [] -- Not using attributes now
        (ModuleAST id exports defs) = mod

compileExports :: [ExportAST] -> AST.Module -> [CES.Function]
compileExports es mod = (map (compileExport mod) es) ++ [mi0,mi1]
  where mi0  = CES.Function (name,0)
        mi1  = CES.Function (name,1)
        name = CES.Atom "module_info"

-- |The 'compileExport' function compiles an Export
--  to a CoreErlang.Syntax.Function
compileExport :: AST.Module -> ExportAST -> CES.Function
compileExport mod (ExportAST id) = CES.Function (CES.Atom id, getArity id mod)

-- |The 'compileFun' function compiles a Def DFun
--  to a CoreErlang.Exp
compileFun :: AST.Module -> DefAst a -> CES.FunDef
compileFun mod (DefAST id _ AST) = FunDef (Constr (getSig id mod)) (Constr (compileAST ast))

-- |The 'compileType' function compiles a Type
--  to CoreErlang data
--  Not implemented
compileType :: TypeAST -> String
compileType (VarType id)    = undefined
compileType (ConType id ts) = undefined

-- |The 'compileExp' function compiles an Exp
--  to a CoreErlang.Exp
--  EInfix should not be dealt with, the updated data structure
--  won't contain infix, will remove when implemented
--  EApp not implemented, ELambda might need verification
compileAST :: AST -> CES.Exp
compileAST (Named id)      = Var id
compileAST (LitStr s)      = Lit (LString s)
compileAST (LitInteger i)  = Lit (LInt i)
compileAST (LitDouble d)   = Lit (LFloat d) -- No double constructor in CoreErlang
compileAST (LitChar d)     = Lit (LChar c) -- No double constructor in CoreErlang
compileAST (LamAST pats a) = Lambda (map compileLambdaPat pats) (CES.Exp (Constr (compileAST a)))
compileAST (AppAST a1 a2)  = undefined 

-- |The 'compileLambdaPat' function converts a
--  lambda pattern to a CoreErlang.Var
compileLambdaPat :: PatAst -> CES.Var
compileLambdaPat (VarPat id) = id
compileLambdaPat PWild       = "_"

-- |The 'getArity' function returns the arity of
--  the function with the given id
--  Takes a String function id and the complete AbsHPR.Module as argument
--  Used when creating the export list
getArity :: String -> ModuleAST -> Integer
getArity id mod = toInteger $ length types - 1
  where (DSig _ types) = getSig id mod

-- |The 'getSig' function returns the DSig with
--  the corresponding id
--  Takes a String function id and the completh AbsHPR.Module as argument
--  This function assumes that the given Module contains
--  a function signature with the given Function Id
getSig :: String -> HPR.Module -> Def
getSig id (MModule mId es (def:defs))
  | id == funId = sig
  | otherwise   = getSig id (MModule mId es defs)
  where (DSig (IdVar funId) types) = sig
        (DCollected _ sig _)       = def

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
