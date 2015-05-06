module Utils.BIF (isBIF, lookupBIF) where

import AST.AST
import Data.Map as M

-- Thinking we want to extend this later
-- Maybe make a record of it?
data BIF = B Identifier Type deriving Show

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
bifs = M.fromList $ ("+",   B "+"   (int ~> int ~> int))
                  : ("-",   B "-"   (int ~> int ~> int))
                  : ("*",   B "*"   (int ~> int ~> int))
                  : ("/",   B "/"   (int ~> int ~> int))
                  : ("<",   B "<"   (int ~> int ~> bool))
                  : (">",   B ">"   (int ~> int ~> bool))
                  : ("==",  B "=:=" (var ~> var ~> bool))
                  : ("/=",  B "=/=" (var ~> var ~> bool))
                  : (">=",  B ">="  (int ~> int ~> bool))
                  : ("<=",  B "<="  (int ~> int ~> bool))
                --  : ("++",  B "++"  (list ~> list ~> list)
                --  : ("!",   B "!"   (atom ~> msg ~> io)
                  : []
  where int  = TCon "Prim.Number"
        bool = TCon "Prim.Bool"
        var  = TVar "a"
        -- string = TCon "Prim.String"
        -- double = TCon "Prim.Double"

(~>) :: Type -> Type -> Type
(~>) a b = (TCon "Prim.->" `TApp` a) `TApp` b
infixr 7 ~>

