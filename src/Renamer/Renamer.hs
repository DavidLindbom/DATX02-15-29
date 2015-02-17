module Renamer.Renamer (collectDefs) where

import qualified Data.Map as M
import Control.Monad (foldM)

import Parser.AbsHopper
import Parser.ErrM

-- main function: collects all defs into [Def] where each Def is DCollected.
collectDefs :: Module -> Err Module
collectDefs (MModule id exps defs) = do
  sigm <- findSigs defs
  funs <- findFuns defs sigm
  return $ MModule id exps (M.foldrWithKey go [] funs)
  where
    go :: Def -> [Def] -> [Def] -> [Def]
    go sig@(DSig id _) funs acc =
       (DCollected id sig funs) : acc

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
           Nothing  -> fail $ "Function " ++ show id ++ " defined without " ++
                              "matching signature"
           Just sig -> return $ M.insertWith insfun sig [def] m
    insfun :: [a] -> [a] -> [a]
    insfun [new] old = new : old
