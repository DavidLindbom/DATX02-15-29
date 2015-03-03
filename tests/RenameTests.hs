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


{-

untransformed :: HPR.Module
untransformed = MModule (IdCon "MyModule") e f
 where e = [MExport (IdVar "a"),MExport (IdVar "b")] 
       f = [DSig (IdVar "a") [HPR.TName (IdCon "Int")
                             ,HPR.TVar (IdVar "b")
                             ,HPR.TName (IdCon "Bool")
                             ,HPR.TName (IdCon "Int")]
           ,DFun (IdVar "a") (HPR.ELambda [HPR.PWild
                                          ,HPR.PWild
                                          ,HPR.PCon (IdCon "True")] (EInteger 0))
           ,DFun (IdVar "a") (HPR.ELambda [HPR.PVar (IdVar "n")
                                          ,HPR.PWild
                                          ,HPR.PWild] (HPR.EVar (IdVar "n")))
           ,DFun (IdVar "b") (HPR.EApp 
                               (HPR.EApp 
                                 (HPR.EApp (HPR.EVar (IdVar "a")) 
                                           (EInteger 4)) 
                                 (EChar 'c')) 
                               (HPR.ECon (IdCon "False")))]

            
transformed = Mod "MyModule" e f 
  where e = ["a","b"] 
        f = [Fun "a" (Just [AST.TName "Int" []
                           ,AST.TVar "b"
                           ,AST.TName "Bool" []
                           ,AST.TName "Int" []]) 
              [AST.ELambda Nothing [AST.PWild
                                   ,AST.PWild
                                   ,AST.PCon "True"] (ELit (Just [AST.TName "Integer" []]) (LI 0))
              ,AST.ELambda Nothing [AST.PVar "n"
                                   ,AST.PWild
                                   ,AST.PWild] (AST.EVar Nothing "n")]


-- Transform
-- module MyModule (f) where
-- f :: Int -> Int -> Bool
-- f 1 2 = True
-- f 2 3 = True
-- f n m = False
in = transform $ MModule (IdCon "MyModule") 
                         [MExport (IdVar "f")] 
                         [DSig (IdVar "f") 
                               [HPR.TName (IdCon "Int")
                               ,HPR.TName (IdCon "Int")
                               ,HPR.TName (IdCon "Bool")]
                         ,DFun (IdVar "f") 
                               [AInteger 1
                               ,AInteger 2] 
                               (HPR.ECon (IdCon "True"))
                         ,DFun (IdVar "f") 
                               [AInteger 2
                               ,AInteger 3] 
                               (HPR.ECon (IdCon "True"))
                         ,DFun (IdVar "f") 
                               [AVar (IdVar "n")
                               ,AVar (IdVar "m")] 
                               (HPR.ECon (IdCon "False"))
                         ]

out = Ok (Mod "MyModule" ["f"] 
          [Fun "f" (Just [TName "Int" []
                         ,TName "Int" []
                         ,TName "Bool" []]) 
                   (ELambda Nothing 
                            [PVar "_arg1"
                            ,PVar "_arg2"] 
                            (ECase Nothing 
                                   (ETuple Nothing 
                                           [EVar Nothing "_arg1"
                                           ,EVar Nothing "_arg2"]) 
                                   [(PTuple [PLit (LI 1),PLit (LI 2)],ECon Nothing "True")
                                   ,(PTuple [PLit (LI 2),PLit (LI 3)],ECon Nothing "True")
                                   ,(PTuple [PCon "n",PCon "m"],ECon Nothing "False")]))])
--}
