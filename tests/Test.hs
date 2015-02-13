module Main (main) where

import Test.HUnit hiding (Test) 
import Test.Framework 
import Test.Framework.Providers.HUnit

import Parser.LayoutHopper (resolveLayout)
import Parser.ParHopper (myLexer, pModule) 
import Parser.ErrM

main :: IO () 
main = defaultMain tests


tests :: [Test]
tests = [ testGroup "Syntax tests" 
            [ testCase "Parse code" (readAndParseCodeFile "tests/Syntax.hpr")
            ]
        ]

readAndParseCodeFile :: FilePath -> Assertion
readAndParseCodeFile s = do
  str <- readFile s
  case parse str of
    Bad msg -> assertFailure msg
    Ok tree -> return ()
  where parse = pModule . resolveLayout True . myLexer
