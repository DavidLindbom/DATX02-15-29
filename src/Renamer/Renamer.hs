module Renamer.Renamer (transformModule) where

import qualified Data.Map as M
import Control.Monad (foldM)

import Parser.AbsHopper
import AST.AST
import Parser.ErrM

transformModule :: Module -> Err (ModuleAST (Maybe TypeAST))
transformModule (MModule (IdCon mname) exports defs) = do
  defs' <- transformDefs defs
  return $ ModuleAST mname (map exportToAST exports) defs'

exportToAST :: Export -> ExportAST
exportToAST (MExport (IdVar s)) = ExportAST s

transformDefs :: [Def] -> Err [DefAST (Maybe TypeAST)]
transformDefs defs = do
  sigm <- findSigs defs
  funs <- findFuns defs sigm
  return $ M.foldrWithKey go [] funs
  where
    go :: Def -> [Def] -> [DefAST (Maybe TypeAST)] -> [DefAST (Maybe TypeAST)]
    go sig funs acc =
       (defsToAST sig funs) : acc

findSigs :: [Def] -> Err (M.Map IdVar Def)
findSigs defs =
  foldM go M.empty defs
  where
    go :: M.Map IdVar Def -> Def -> Err (M.Map IdVar Def)
    go m (DFun _ _)      = return m
    go m def@(DSig id _) =
      case M.lookup id m of
           Nothing -> return $ M.insert id def m
           Just _  -> fail $ "Multiple signatures for " ++ show id

findFuns :: [Def] -> M.Map IdVar Def -> Err (M.Map Def [Def])
findFuns defs sigm =
  foldM go M.empty (reverse defs)
  where
    go m (DSig _ _)      = return m
    go m def@(DFun id _) =
      case M.lookup id sigm of
           Nothing  -> fail $ "Function " ++ show id ++
                              " defined without " ++ "matching signature"
           Just sig -> return $ M.insertWith insfun sig [def] m
    insfun :: [a] -> [a] -> [a]
    insfun [new] old = new : old

--           sig    bindings
defsToAST :: Def -> [Def] -> DefAST (Maybe TypeAST)
defsToAST (DSig (IdVar id) ts) (DFun _ ex : ds) =
  -- TODO when grammar has progressed: transform multiple bindings with
  -- pattern matching into case here. currently we do not have pattern
  -- matching in function bindings (since we do not have arguments), so the
  -- current behaviour is to take the first binding and disregard all
  -- following ones. E.g.:
  -- f :: Int
  -- f = 2
  -- f = 4
  -- Here, "f = 4" is thrown away.
  DefAST id (Just $ typesToAST ts) (expToAST ex)

typesToAST :: [Type] -> TypeAST
typesToAST [t] = typeToAST t
typesToAST ts  = ConType "->" (map typeToAST ts)

-- helper
typeToAST :: Type -> TypeAST
typeToAST (TName (IdCon t)) = ConType t []
typeToAST (TVar (IdVar v))  = VarType v

expToAST :: Exp -> AST
expToAST e = case e of (EVar (IdVar s)) -> Named s
                       (ECon (IdCon s)) -> Named s
                       (EOpr (IdOpr s)) -> Named s
                       (EString s) -> LitStr s
                       (EChar c) -> LitChar c
                       (EInteger i) -> LitInteger i
                       (EDouble d) -> LitDouble d
                       (EInfix e1 (IdOpr op) e2) -> (Named op) `AppAST` 
                                                    expToAST e1 `AppAST`
                                                    expToAST e2
                       (EApp f x) -> AppAST (expToAST f) $ expToAST x
                       (ELambda ps x) -> LamAST (map patToAST ps) $ expToAST x

patToAST :: Pat -> PatAST
patToAST (PCon (IdCon s)) = VarPat s -- this is probably not what we want to do!
patToAST (PVar (IdVar s)) = VarPat s
patToAST PWild = WildPat
