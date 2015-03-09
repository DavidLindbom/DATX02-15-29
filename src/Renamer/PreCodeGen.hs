module Renamer.PreCodeGen (transform2) where

import AST.AST
import Utils.ErrM

-- |The 'transform2' function transforms a Signature annotated
--  module to a module of the same structure, where expressions
--  are transformed to fit the codegenerator
transform2 :: Module Signature -> Err (Module Signature)
transform2 m@(Mod mId es funs) = Ok $ Mod mId es $ map transformFun funs
 
-- |The 'transformFun' function transforms a Signature annotated
--  function to a function of the same structure, where expressions
--  are transformed to fit the codegenerator
transformFun :: Function Signature -> Function Signature
transformFun (Fun i sig e) = Fun i sig $ transformExp [] e

-- |The 'transformExp' function transforms an Expression
--  to an expression fit for the codegenerator. This means that
--  EApps are unfolded to EVals, and the expression of an ECall will
--  a tuple consisting of its arguments
transformExp :: [Pattern] -> Expression -> Expression
transformExp scope e@(EVar i)       =
  if isIdBound i scope
     then e
     else EVal i []
transformExp scope (ETuple es)      = ETuple $ map (transformExp scope) es
transformExp scope (ELambda pats e) = ELambda pats $ transformExp (addScope pats scope) e
transformExp scope e@(EApp _ _)     = transformEApp e [] scope
transformExp scope (ECase e cases)  = ECase e' cases'
  where e'     = transformExp scope e
        cases' = map (\(p,ce) -> (p, transformExp (addScope [p] scope) ce)) cases
transformExp scope (ECall m f e)    = undefined
transformExp _     e                = e

-- |The 'transformEApp' function unfolds an EApp
--  to EVals and recursively calls transformExp on the arguments
transformEApp :: Expression -> [Expression] -> [Pattern] -> Expression
transformEApp (EApp l@EApp{}      r) args scope = transformEApp l (r:args) scope
transformEApp (EApp l@(EVar i)    r) args scope = 
  if not (isIdBound i scope)
     then EVal i (map (transformExp scope) (r:args))
     else transformEApp l (r:args) scope
transformEApp (EApp (ECall m f e) r) args scope =
  ECall m f (ETuple (map (transformExp scope) (e:(r:args))))
transformEApp e                      _    _     = error $ "Non allowed exp in EApp: " ++ show e

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
