module Utils.BIF (isBIF, lookupBIF) where

import AST.AST
import Data.Map as M

-- Thinking we want to extend this later
-- Maybe make a record of it?
data BIF = B Identifier Type

isBIF :: Identifier -> Bool
isBIF i = M.member i bifs

lookupBIF :: Identifier -> Maybe (Identifier, Identifier, Type)
lookupBIF i = case M.lookup i bifs of
  Nothing        -> Nothing
  Just (B f s) -> Just ("erlang",f,s)

-- TODO: Fill this map in...
-- The first element of the tuple is the hopper alias 
-- for the erlang function. Most of them will be the same but 
-- some need to be different, e.g functions with same name but 
-- different arity.
bifs :: M.Map Identifier BIF
bifs = M.fromList $ ("+", B "+" (int `TApp` int `TApp` int))
                  : ("-", B "-" (int `TApp` int `TApp` int))
                  : []
  where int = TCon "Prim.Int"
        -- string = TCon "Prim.String"
        -- double = TCon "Prim.Double"
