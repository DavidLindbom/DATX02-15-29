module Utils.HipFile (storeModuleInfo, getModuleInfo) where

import Control.Monad (liftM)
import Data.Map as Map

import AST.AST
import Utils.PathTools

storeModuleInfo :: Modulename -> Map.Map Identifier Type -> Map.Map Constructor Type -> IO ()
storeModuleInfo m funs ts =
  writeFile (toFilePath m) (show (funs, ts))

getModuleInfo :: Modulename -> IO (Map.Map Identifier Type, Map.Map Constructor Type)
getModuleInfo m =
  liftM read $ readFile (toFilePath m)
