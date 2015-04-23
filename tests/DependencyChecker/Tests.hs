module DependencyChecker.Tests (dependencyCheckerTests) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import qualified Data.Map as Map
import Data.Graph
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
import System.Directory
import System.FilePath
import System.IO
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
      [ testCase "Check valid module with no imports" checkNoImports ]
    testGroupReccheck = []
    testGroupAcyclicOrder = []
    testGroupDependencyCheck = []

checkNoImports :: Assertion
checkNoImports = do
  fp' <- makeAbsolute fp
  putStrLn fp'
  themap <- execStateT (check fp') emptyEnv
  assertBool "Size of map not one" $ Map.size themap == 1
  where
    fp = good "NoImports"


good :: String -> FilePath
good m = "Good" </> m <.> hopExt

bad :: String -> FilePath
bad m = "Bad" </> m <.> hopExt
