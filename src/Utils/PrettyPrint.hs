module Utils.PrettyPrint where

import AST.AST

showSignature :: Signature -> String
showSignature (t:ts) = go t ++ concatMap (\t' -> " -> " ++ go t') ts
  where go (TName a as)    = a ++ concatMap (\a' -> " " ++ go a') as
        go (TVar a as)     = a ++ concatMap (\a' -> " " ++ go a') as
        go (TTuple (a:as)) = "( " ++ go a ++ concatMap (\a' -> " -> " ++ go a') as ++ " )"
