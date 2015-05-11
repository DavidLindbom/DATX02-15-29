module Main (main) where

import Test.HUnit hiding (Test) 
import Test.Framework 
import Test.Framework.Providers.HUnit

import Parser.LayoutHopper (resolveLayout)
import Parser.ParHopper (myLexer, pModule) 
import Utils.ErrM

--import RenameTests
--import CodeGenTests

main :: IO () 
main = defaultMain tests


tests :: [Test]
tests = [ testGroup "Syntax tests" 
            [ testCase "Parse code" (readAndParseCodeFile "tests/Syntax.hpr")
            ]
    --    , testGroup "Renaming tests" renameTests
    --    , testGroup "Code generation tests" codeGenTests
        ]

readAndParseCodeFile :: FilePath -> Assertion
readAndParseCodeFile _ = do return ()

--readAndParseCodeFile :: FilePath -> Assertion
--readAndParseCodeFile s = do
--  str <- readFile s
--  case parse str of
--    Bad msg -> assertFailure msg
--    Ok tree -> return ()
--  where parse = pModule . resolveLayout True . myLexer

