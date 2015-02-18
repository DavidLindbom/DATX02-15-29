module CodeGenTests (codeGenTests) where

import Test.HUnit hiding (Test)

import Test.Framework
import Test.Framework.Providers.HUnit

import Parser.AbsHopper as HPR
import Parser.PrintHopper
import Parser.ErrM
import CodeGenerator.CodeGenerator
import CodeGenerator.BeamWriter
import Language.CoreErlang.Syntax as CES

-- |Test that verifies the quality of the code generator
codeGenTests :: [Test]
codeGenTests =
  [ testCase "Testing getSig" (testGetSig id (tSig id)) | id <- ["e", "a", "b"] ] ++
  [ testCase "Testing getArity" (testGetArity id ar) | (id,ar) <- [("e",0),("a",1),("b",3)] ]

-- |Test that checks that the getSig function
--  return the correct Def DSig for the function with
--  the given id
testGetSig :: String -> HPR.Def -> Assertion
testGetSig id expectedDef =
  if receivedSig == expectedSig
     then return ()
     else assertFailure $ failmsg expectedSig receivedSig
  where receivedSig = getSig id tModule
        failmsg d1 d2 "Expected sig:\n" ++ (printTree d1) ++ "\nGot:\n" ++ (printTree d2)

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
tSig :: String -> HPR.Def
tSig "e" = (DSig (IdVar "e") [TName (IdCon "Int")])
tSig "a" = (DSig (IdVar "a") [TName (IdCon "String"),TName (IdCon "String")])
tSig "b" = (DSig (IdVar "b")
              [ TName (IdCon "Int")
              , TName (IdCon "Int")
              , TName (IdCon "String")
              , TName (IdCon "String")
              ]
           )

-- |Returns a module to use in testing
tModule :: HPR.Module
tModule = MModule (IdCon "codeGenTest")
            [ MExport (IdVar "e"),
              MExport (IdVar "a"),
              MExport (IdVar "b")
            ]
            [ DCollected
                (IdVar "e")
                (DSig (IdVar "e") [TName (IdCon "Int")])
                [DFun (IdVar "e") (EInteger 1)]
            , DCollected
                (IdVar "a")
                (DSig (IdVar "a") [TName (IdCon "String"),TName (IdCon "String")])
                [DFun (IdVar "a") (EInteger 1)]
            , DCollected
                (IdVar "b")
                (DSig (IdVar "b")
                   [ TName (IdCon "Int")
                   , TName (IdCon "Int")
                   , TName (IdCon "String")
                   , TName (IdCon "String")
                   ]
                )
                [DFun (IdVar "b") (EInteger 1)]
            ]
