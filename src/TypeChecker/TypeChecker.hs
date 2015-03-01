-- Hopper using the Hindley-Milner (HM) type system.
-- It is a type system for lambda calculus with parametric polymorphism.
-- The programmer does not need to give any type signatures, the algorithm can
-- reconstruct (infer) the type of any expression in the language. Instead type
-- signatures are used as a means of specification.  (Types as specification).
-- "The purpose of the notion [of type] in functional programming is to assure
-- us at compile-time that a program will not 'go wrong' [at run-time], where
-- we do not count a program to have gone wrong if it does not terminate,
-- or a function is applied to arguments for which it has not been defined."
-- - The Implementation of Functional Programming Languages, p. 162
-- To 'go wrong' we need to add the 'recieve' construct (and others?) for which
-- we do not know the type of the recieved data until runtime.
-- A type system is a formal system where type checking amounts to proving the
-- type of an expression. (Inference rules, judgments)
-- Based on "Algorithhm W Step by Step" by Martin Grabmüller available at
-- https://github.com/wh5a/Algorithm-W-Step-By-Step

module TypeChecker.TypeChecker (typeCheck) where

import AST.AST

typeCheck :: Module (Maybe Signature) -> Module Signature
typeCheck = noCheck

-- noCheck performes no type checking, assumes given types are correct and
-- simply substitutes every Nothing with the empty list. noCheck allows
-- the type checker to be in the compiler pipeline while under development.
noCheck :: Module (Maybe Signature) -> Module Signature
noCheck (Mod name exported functions) = Mod name exported $ map noFun functions
  where
    subst :: Maybe Signature -> Signature
    subst (Just s) = s
    subst Nothing  = []
    -- noFun performs substitution in function definitions.
    noFun :: Function (Maybe Signature) -> Function Signature
    noFun (Fun ident mt exprs) = Fun ident (subst mt) (map noExp exprs)
    -- noExp performs substitution in expressions
    noExp :: Expression (Maybe Signature) -> Expression Signature
    noExp expr = case expr of
      EVar    mt i        -> EVar    (subst mt) i
      ECon    mt c        -> ECon    (subst mt) c
      ELit    mt l        -> ELit    (subst mt) l
      ELambda mt ps e     -> ELambda (subst mt) ps (noExp e)
      EApp    mt e1 e2    -> EApp    (subst mt) (noExp e1) (noExp e2)
      -- EWhere  fs          -> EWhere  (map noFun fs)
      ECase   cs          -> ECase   (map (\(ps, e) -> (ps, noExp e)) cs)
      -- ECall   mt i1 i2 es -> ECall   mt i1 i2 (map noExp es)
      -- ELet    ps e1 e2    -> ELet    ps (noExp e1) (noExp e2)
      
