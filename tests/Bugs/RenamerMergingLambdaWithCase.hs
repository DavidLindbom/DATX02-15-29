-- A problem when merging to definitions and the first one's expression
-- was a case. The problem was then that it added the second definition
-- as a new clause to the existing case instead of generating 
-- a new case expression.
--
-- Minimal reproduceble module:
--
--     module Bug1 where
--     f a = case a of a -> a
--     f a = a
--
-- Fixed in commit #d05abc9
module RenamerMergingLambdaWithCase (test) where

import Parser.AbsHopper as HPR
import AST.AST as AST
import Renamer.Renamer
import Utils.ErrM 
import Data.Map as M

test = (transform input) == output

input = MMod (IdCon "Bug1") NEmpty [DFun (FFun (IdVar "f") [APat (PId (IVar (IdVar "a")))] (HPR.ECase (EId (IVar (IdVar "a"))) [CClause (CCPPat (PId (IVar (IdVar "a")))) (EId (IVar (IdVar "a")))])),DFun (FFun (IdVar "f") [APat (PId (IVar (IdVar "a")))] (EId (IVar (IdVar "a"))))]
output = Ok (Mod "Bug1" ["f"] [Fun "f" Nothing (AST.ELambda [PVar "_arg1"] (AST.ECase (AST.ETuple [EVar "_arg1"]) [(AST.PTuple [PVar "a"],AST.ECase (ECon "a") [(PVar "a",ECon "a")]),(AST.PTuple [PVar "a"],ECon "a")]))] (M.fromList []))
