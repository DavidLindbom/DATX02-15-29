module DependencyChecker.Tests (dependencyCheckerTests) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import qualified Data.Map as Map
import Data.Graph
import qualified Data.List as List
import Data.Maybe (fromJust)
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
import System.Directory
import System.FilePath
import System.IO
import qualified System.IO.Error as IOErr
import System.Process

import DependencyChecker.Internal.DependencyChecker
import Utils.PathTools

dependencyCheckerTests :: [Test]
dependencyCheckerTests =
  [ testGroup "check" testGroupCheck
  , testGroup "reccheck" testGroupReccheck
  , testGroup "acyclicOrder" testGroupAcyclicOrder
  , testGroup "dependencyCheck" testGroupDependencyCheck ]
  where
    testGroupCheck =
      [ testCase "Check valid module with no imports" checkNoImports
      , testCase "Check valid module with three imports" checkImports
      , testCase "Check module with invalid name" checkInvalidName
      , testCase "Check module that doesn't exist" checkMissingModule ]
    testGroupReccheck = []
    testGroupAcyclicOrder = []
    testGroupDependencyCheck = []

-- | Test of a module with no imports.
-- Check that
--   the size of the resulting map is one
--   the given file path is the key
--   the import list from the map is empty
--   the import list from the return value is empty
checkNoImports :: Assertion
checkNoImports = do
  fp' <- makeAbsolutePath $ good "NoImports"
  (imps, themap) <- runStateT (check fp') emptyEnv
  assertBool "Size of map not one" $ Map.size themap == 1
  assertBool "File path key not found" $ Map.member fp' themap
  assertBool "Import list in map not empty"
    $ null $ fromJust $ Map.lookup fp' themap
  assertBool "Import list in returned value not empty" $ null imps
  
-- | Test of a module which imports three modules.
-- Check that
--   the resulting map size is one
--   the import list from the return value has three elements
--   the generated file paths are correct
checkImports :: Assertion
checkImports = do
  fp' <- makeAbsolutePath $ good "ImportNoImports"
  (imps, themap) <- runStateT (check fp') emptyEnv
  assertBool "Size of map not one" $ Map.size themap == 1
  assertBool "Import list from return of wrong size" $ 3 == length imps
  mapM_ doesExist imps
  where
    doesExist :: FilePath -> Assertion
    doesExist fp = do
      exists <- doesFileExist fp
      assertBool ("Created bad file path: " ++ fp) $ not exists

-- | Test error is generated for module where the module name doesn't match
-- the file path.
-- Check that error is generated
checkInvalidName :: Assertion
checkInvalidName = do
  fp' <- makeAbsolutePath $ bad "InvalidName"
  res <- IOErr.tryIOError $ execStateT (check fp') emptyEnv
  case res of
    Left err -> do
      let msg = IOErr.ioeGetErrorString err
      when (not $ "Bad module name" `List.isInfixOf` msg)
        $ assertFailure "Reported unknown error"
    Right _  -> assertFailure "Did not report bad module name"

-- | Test error is generated for missing module.
-- Check that erro is generated
checkMissingModule :: Assertion
checkMissingModule = do
  fp' <- makeAbsolutePath $ bad "Hopper"
  res <- IOErr.tryIOError $ execStateT (check fp') emptyEnv
  case res of
    Left err -> when (not $ IOErr.isDoesNotExistError err)
                  $ assertFailure "Reported unknown error"
    Right _  -> assertFailure "Did not report missing module"

good :: String -> FilePath
good m = "Good" </> m <.> hopExt

bad :: String -> FilePath
bad m = "Bad" </> m <.> hopExt
