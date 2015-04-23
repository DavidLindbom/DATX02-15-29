module Main where

import Test.HUnit hiding (Test)

import Test.Framework
import Test.Framework.Providers.HUnit

import DependencyChecker.Tests

main :: IO () 
main = defaultMain dependencyCheckerTests
