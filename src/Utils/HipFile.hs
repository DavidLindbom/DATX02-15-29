module Utils.HipFile (storeModuleInfo, getImportsInfo) where

import System.FilePath

import Control.Monad (liftM)
import qualified Data.Map as Map
import Data.Char (isLower)

import AST.AST
import Utils.PathTools
import Utils.ErrM

-- | Store export type information for a given module.
storeModuleInfo :: Err (Module Type) -> IO ()
storeModuleInfo (Ok m@(Mod name _ _ _ _)) =
  let (funs, adts) = getExports m
   in writeFile (replaceExtension (toFilePath name) "hip") (show (funs, adts))

-- | Get type information for all exported functions and ADTs from all imports
-- of a given module, aggregated into two large maps.
getImportsInfo :: Err (Module a)
               -> IO (Map.Map Identifier Type,  -- functions
                      Map.Map Constructor Type) -- ADTs
getImportsInfo (Ok (Mod _ _ imps _ _)) =
  getModulesInfo (map toFilePath imps)

-- | Get module info for a given .hpr file.
-- This function *expects* that the interface file already exists, since
-- dependencies should be compiled before the dependant module.
getModuleInfo :: FilePath -> IO (Map.Map Identifier Type, Map.Map Constructor Type)
getModuleInfo fp =
  liftM read $ readFile (replaceExtension fp "hip")

-- | getModuleInfo for a number of filepaths at the same time. Essentially
-- `map`, but a little more complicated due to the Maps involved.
getModulesInfo :: [FilePath] -> IO (Map.Map Identifier Type, Map.Map Constructor Type)
getModulesInfo fps = do
  maps <- mapM getModuleInfo fps -- maps :: [(Map..., Map...)]
  return $ (\(funss, adtss) -> (Map.unions funss, Map.unions adtss)) (unzip maps)

-- | Dummymap contains the keys in the export list, and is used to find
-- matching elements in the module's function and ADT maps
getExports :: Module Type -> (Map.Map Identifier Type, Map.Map Constructor Type)
getExports (Mod name exps _ defs adts) =
  (Map.intersection defmap dummymap, Map.intersection adts dummymap)
  where
    defmap   = Map.fromList $ map (\(Fun name typ _) -> (name, typ)) defs
    dummymap = Map.fromList $ map (\k -> (k, undefined)) exps
