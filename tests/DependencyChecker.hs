module Main where

import System.Directory
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import DependencyChecker.Tests

main :: IO () 
main = do
  dir <- getCurrentDirectory
  setCurrentDirectory $ dir </> "tests" </> "DependencyChecker"
  defaultMain dependencyCheckerTests
