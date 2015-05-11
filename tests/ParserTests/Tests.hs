module ParserTests.Tests (parserTests) where

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

import Parser.Parser
import Utils.PathTools
import Utils.ErrM

parserTests :: [Test]
parserTests =
  [ testGroup "good" goodTests
  , testGroup "bad" badTests ]

badTests :: [Test]
badTests =
  map (\(name, descr) -> testCase descr (checkBad name)) cases
  where
  cases = [ ("Bad01", "lower case module name")
          , ("Bad02", "missing \"where\" after module name")
          , ("Bad03", "import lower case module name")
          , ("Bad04", "lowercase part of qualified module name")
          , ("Bad05", "import invalid qualified module name")
          , ("Bad06", "type signature with upper case name")
          , ("Bad07", "qualified type variable")
          ]

goodTests :: [Test]
goodTests =
  map (\(name, descr) -> testCase descr (checkGood name)) cases
  where
  cases = [ ("Good01", "simplest module possible")
          , ("Good02", "export ADTs")
          , ("Good03", "export ADTs and functions")
          , ("Good04", "qualified module name")
          , ("Good05", "type signature and function definition")
          , ("Good06", "qualified types in type signature")
          , ("Good07", "import statement")
          , ("Good08", "import qualified modules")
          , ("Good09", "imports together with definitions")
          , ("Good10", "export qualified names")
          ]

checkBad :: String -> Assertion
checkBad name = do
  fp <- makeAbsolutePath $ "tests" </> "ParserTests" </> "Bad" </> name <.> "hpr"
  contents <- readFile fp
  case parse contents of
       Ok _  ->
         assertFailure $ "Bad test case " ++ name ++ " accepted by parser!"
       Bad _ ->
         return ()

checkGood :: String -> Assertion
checkGood name = do
  fp <- makeAbsolutePath $ "tests" </> "ParserTests" </> "Good" </> name <.> "hpr"
  contents <- readFile fp
  case parse contents of
       Bad _ ->
         assertFailure $ "Good test case " ++ name ++ " rejected by parser!"
       Ok _  ->
         return ()
