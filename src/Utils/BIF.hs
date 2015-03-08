module Utils.BIF (isBIF, lookupBIF) where

import AST.AST
import Data.Map as M

-- Thinking we want to extend this later
-- Maybe make a record of it?
data BIF = B Identifier Signature

isBIF :: Identifier -> Bool
isBIF i = M.member i bifs

lookupBIF :: Identifier -> Maybe (Identifier, Identifier, Signature)
lookupBIF i = case M.lookup i bifs of
  Nothing        -> Nothing
  Just (B f s) -> Just ("erlang",f,s)

-- TODO: Fill this map in...
bifs :: M.Map Identifier BIF
bifs = M.fromList $ ("+", B "+" [tn "Int", tn "Int", tn "Int"])
                  : ("-", B "-" [tn "Int", tn "Int", tn "Int"])
                  : []
  where tn n = TName n []
