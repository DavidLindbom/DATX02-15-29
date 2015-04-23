module DependencyChecker.Tests (dependencyCheckerTests) where

import Test.HUnit hiding (Test)

import Test.Framework
import Test.Framework.Providers.HUnit
import System.Directory
import System.Process
import System.IO

import DependencyChecker.Internal.DependencyChecker

dependencyCheckerTests :: [Test]
dependencyCheckerTests =
  [ testCase "toFilePath" toFilePathTest1
  ]

toFilePathTest1 :: Assertion
toFilePathTest1 = assertFailure "Test not implemented yet"
