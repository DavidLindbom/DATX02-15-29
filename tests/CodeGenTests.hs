module CodeGenTests (codeGenTests) where

import Test.HUnit hiding (Test)

import Test.Framework
import Test.Framework.Providers.HUnit

import AST.AST as HPR
import Parser.PrintHopper
import Utils.ErrM
import CodeGenerator.CodeGenerator
import Utils.BeamWriter
import Language.CoreErlang.Syntax as CES
import Language.CoreErlang.Pretty as CEP
import System.Directory
import System.Process
import System.IO

-- |Test that verifies the quality of the code generator
codeGenTests :: [Test]
codeGenTests =
  [ testCase "Testing compiling and running beam" testCompiled ] ++
  [ testCase "Testing getTypeSig" (testGetTypeSig id (tSig id)) | id <- ["i", "s"] ] ++
  [ testCase "Testing getArity" (testGetArity id ar) | (id,ar) <- [("i",0),("s",0)] ]

-- |Test that a module compiles and functions as expected
testCompiled :: Assertion
testCompiled =
  case cm of
    (Ok cesMod) ->
      do writeBeam modName cesMod False
         resultI <- readProcess "erl" (pArgs "i") ""
         resultS <- readProcess "erl" (pArgs "s") ""
         removeFile $ modName ++ ".beam"
         if resultI == "10" && resultS == "\"test\"" --Erlangs ~p printing prints strings with surrounding ""
            then return ()
            else assertFailure $ failmsg resultI resultS
    (Bad s) -> do assertFailure s
  where modName = "CodeGenTest"
        cm = compileModuleString tModule
        failmsg i s = "Expected output: 10 and test, got: " ++ i ++ " and " ++ s
        pArgs f = [ "-noshell"
                  , "-eval" 
                  , "io:format(\"~p\", [\'" ++ modName ++ "\':" ++ f ++ "()]),init:stop()"
                  ]

-- |Test that checks that the getSig function
--  return the correct Def DSig for the function with
--  the given id
testGetTypeSig :: String -> Signature -> Assertion
testGetTypeSig id expectedSig =
  if receivedSig == expectedSig
     then return ()
     else assertFailure $ failmsg expectedSig receivedSig
  where receivedSig   = getTypeSig id tModule
        failmsg s1 s2 ="Expected sig:\n" ++ show s1 ++ "\nGot:\n" ++ show s2

-- |Test that checks that the getArity function
--  return the correct arity for the function with
--  the given id
testGetArity :: String -> Integer -> Assertion
testGetArity id expectedArity =
  if receivedArity == expectedArity
     then return ()
     else assertFailure $ failmsg expectedArity receivedArity
  where receivedArity = getArity id tModule
        failmsg x y   = "Expected arity: " ++ show x ++ " got: " ++ show y

-- |Returns a function signature to use in testing
--  with the given id
tSig :: String -> Signature
tSig "i" = [TName "Int" []]
tSig "s" = [TName "String" []]

-- |Returns a module to use when testing
--  actual compilation
tModule :: HPR.Module Signature
tModule = Mod "CodeGenTest"
            [ "i"
            , "s"
            ]
            [ HPR.Fun "i"
                [TName "Int" []]
                (ELit [TName "Int" []] (LI 10))
            , HPR.Fun "s"
                [TName "String" []]
                (ELit [TName "String" []] (LS "test"))
            ]
