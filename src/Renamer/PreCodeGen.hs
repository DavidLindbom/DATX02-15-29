module Renamer.Renamer (transform2) where

import qualified Data.Map as M
import Control.Monad 

import AST.AST

-- |The 'transform2' function transforms a Signature annotated
--  module to a module of the same structure, where expressions
--  are transformed to fit the codegenerator
transform2 :: Module Signature -> Module Signature
transform2 m@(Mod mId es funs) = Mod mId es $ map (transformFun ars) funs
  where ars = getArities m
 
-- |The 'transformFun' function transforms a Signature annotated
--  function to a function of the same structure, where expressions
--  are transformed to fit the codegenerator
transformFun :: [(Identifier, Integer)] -> Function Signature -> Function Signature
transformFun ars (Fun id sig e) = Fun id sig $ transformExp [] ars e

-- |The 'transformExp' function transforms an Expression
--  to an expression fit for the codegenerator. This means that
--  EApps are unfolded to EVals, and the expression of an ECall will
--  a tuple consisting of its arguments
transformExp :: [Pattern] -> [(Identifier, Integer)] -> Expression -> Expression
transformExp scope _ e@(EVar id)        =
  if isIdBound id scope
     then e
     else EVal id []
transformExp scope ars (ETuple es)      = ETuple $ map (transformExp scope ars) es
transformExp scope ars (ELambda pats e) = ELambda pats $ transformExp (addScope pats scope) ars e
transformExp scope ars e@(EApp _ _)     =
  let (EApp e1 e2) = rotateEApp e in
      case transformExp scope ars e1 of
        EVal id _ -> EVal id (unfoldEAppArgs e2 (getArity id ars) scope ars)
transformExp scope ars (ECase e cases)  = ECase e' cases'
  where e'     = transformExp scope ars e
        cases' = map (\(p,ce) -> (p, transformExp (addScope [p] scope) ars ce)) cases
transformExp scope ars (ECall m f e)    = undefined
transformExp _     _   e                = e

-- |The 'unfoldEAppArgs' attempts to unfold an EApp into
--  a list of expressions with a length of the given
--  Integer. The idea is that this function should be
--  called with a known arity
unfoldEAppArgs :: Expression -> Integer -> [Pattern] -> [(Identifier, Integer)] -> [Expression]
unfoldEAppArgs e 1            _  _  = [e]
unfoldEAppArgs (EApp e1 e2) n sc si = transformExp sc si e1:unfoldEAppArgs e2 (n-1) sc si
unfoldEAppArgs _            _ _  _  = error $ "Error while parsing args for function call"

-- |The 'rotateEApp' rotates a left oriented EApp chain to a
--  right oriented EApp chain
rotateEApp :: Expression -> Expression
rotateEApp (EApp (EApp x y) z) = rotateEApp (EApp x (EApp y z))
rotateEApp e                   = e

-- |The 'isIdBound' function checks if the given id is
--  bound in the given scope
isIdBound :: String -> [Pattern] -> Bool
isIdBound _ [] = False
isIdBound i (PVar i':pats)
  | i == i' = True
  | otherwise  = isIdBound i pats
isIdBound i (_:pats) = isIdBound i pats

-- |The 'addScope' function adds all patterns in the given
--  pattern list to the given scope. Note that tuple pattern
--  is broken down to its primitive patterns
addScope :: [Pattern] -> [Pattern] -> [Pattern]
addScope []     scope = scope
addScope (p:ps) scope = 
  case p of
    PTuple tps -> addScope ps (addScope tps scope)
    _          -> addScope ps (p:scope)

-- |The 'getArities' function gets the function
--  arities from the give module
getArities :: Module Signature -> [(Identifier, Integer)]
getArities (Mod _ _ defs) = map (\(Fun i sig _) -> (i, toInteger (length sig - 1))) defs

-- |The 'getArity' functons gets the arity of the
--  function with the given id from the given list of
--  id and arity pairs
getArity :: Identifier -> [(Identifier, Integer)] -> Integer
getArity i []          = error $ "Could not find function with id: " ++ show i
getArity i ((i',ar):ars)
  | i == i'   = ar
  | otherwise = getArity i ars
