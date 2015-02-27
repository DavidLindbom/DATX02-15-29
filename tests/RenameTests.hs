module RenameTests (renameTests) where

import Test.HUnit hiding (Test)

import Test.Framework
import Test.Framework.Providers.HUnit

import Parser.AbsHopper
import Parser.PrintHopper
import Parser.ErrM
import Renamer.Renamer

--
-- TODO: New tests
-- * Multiple signatures
-- * Missing signature
-- * Keeping order 
--

renameTests :: [Test]
renameTests =
  [testCase s (doRenameTest ms) | (s,ms) <- [("Basic rename", (in1, out1))]]

in1 :: Module
in1 =
    MModule (IdCon "RenameTest") [MExport (IdVar "b")]
      [ DSig (IdVar "a") [TName (IdCon "Int"),TName (IdCon "Int")]
      , DFun (IdVar "a") (EInfix (EInteger 1) (IdOpr "+") (EInteger 1))
      , DSig (IdVar "b") [TName (IdCon "Double"),TName (IdCon "Bool")]
      , DFun (IdVar "b") (EInfix (EInteger 2) (IdOpr "+") (EInteger 3))
      , DFun (IdVar "a") (EInfix (EInteger 2) (IdOpr "+") (EInteger 4))
      , DFun (IdVar "b") (EInfix (EInteger 1) (IdOpr "+") (EInteger 5))
      ]

out1 :: Module
out1 =
    MModule (IdCon "RenameTest") [MExport (IdVar "b")]
      [ DCollected
          (IdVar "a")
          (DSig (IdVar "a") [TName (IdCon "Int"),TName (IdCon "Int")])
          [ DFun (IdVar "a") (EInfix (EInteger 1) (IdOpr "+") (EInteger 1))
          , DFun (IdVar "a") (EInfix (EInteger 2) (IdOpr "+") (EInteger 4))
          ]
      , DCollected
          (IdVar "b")
          (DSig (IdVar "b") [TName (IdCon "Double"),TName (IdCon "Bool")])
          [ DFun (IdVar "b") (EInfix (EInteger 2) (IdOpr "+") (EInteger 3))
          , DFun (IdVar "b") (EInfix (EInteger 1) (IdOpr "+") (EInteger 5))
          ]
      ]

doRenameTest :: (Module, Module) -> Assertion
doRenameTest (ma,mb) =
  case collectDefs ma of
       Bad msg -> assertFailure msg
       Ok ma'  -> if ma' == mb
                     then return ()
                     else assertFailure $ failmsg ma' mb
  where failmsg m1 m2 = "Expected:\n" ++ (printTree m2) ++ "\nGot:\n" ++ (printTree m1)
