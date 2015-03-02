module TypeChecker.TypeChecker (typeCheck) where

{-| 
Module      : TypeChecker
Description : Type checks Hopper abstract syntax trees
Copyright   : -
License     : -
Status      : Partly implemented, non-working. There is an dummy type checker
              which can be used 

  Hopper using the Hindley-Milner (HM) type system.
  It is a type system for lambda calculus with parametric polymorphism.
  The programmer does not need to give any type signatures, the algorithm can
  reconstruct (infer) the type of any expression in the language. Instead type
  signatures are used as a means of specification.  (Types as specification).
  "The purpose of the notion [of type] in functional programming is to assure
  us at compile-time that a program will not 'go wrong' [at run-time], where
  we do not count a program to have gone wrong if it does not terminate,
  or a function is applied to arguments for which it has not been defined."
  - The Implementation of Functional Programming Languages, p. 162
  To 'go wrong' we need to add the 'recieve' construct (and others?) for which
  we do not know the type of the recieved data until runtime.
  A type system is a formal system where type checking amounts to proving the
  type of an expression. The proofs are carried out using inference rules over
  the abstract syntax tree. These rules are sometimes called judgments.
  (Inference rules, judgments)
  Based on "Algorithhm W Step by Step" by Martin Grabmüller available at
  https://github.com/wh5a/Algorithm-W-Step-By-Step
  The type checking process of algorithm W:
  1. 
  The AST described by Grabmüller is a subset of the one defined in Hopper.
  For instance there is a notion of module in Hopper where Grabmüller only
  type checks expressions one at a time. Another significant difference is that
  algorithm W abstracts lambdas over one variable at a time where Hopper allows
  a list of variables to be abstracted over.

  Appendix I:
  monotype - A fully specified type (Int, [String], Int -> Int, Map Char Char)
  polytype - A partially applied type (a, [a] -> Int, (a -> b) -> [a] -> [b])
  inference rule - Tells us what type-conclusion we can draw from types from
                   the sub expressions.
  judgment - see inference rule
  free type variables - Type variables not bound by a quantifier.
  unification - A function that given two types either fails or
                returns a type equal to both the given types.
  specialization - A relation between types signifying one being more 
                   general and the other more "special"
  instantiation - "Specialization of types".
                  An expression of type a is instantiated to type b
                  if type a is more general than type b.
  generalization - "Generalization of types".
                   An expression e of type T in environment Gamma is
                   generalized to type (forall a.T) where a is a type
                   variable not free in Gamma 

This module is part of the Hopper language project
-}

import qualified Data.Map   as Map
import qualified Data.Set   as Set
import qualified Data.Maybe as Maybe
import Control.Monad.Trans.Except
import Control.Monad.Reader
import Control.Monad.State

import qualified Text.PrettyPrint as PP

import AST.AST
import Parser.ErrM

typeCheck :: Module (Maybe Signature) -> Err (Module Signature)
typeCheck m = Ok $ noCheck m

-- noCheck performes no type checking, assumes given types are correct and
-- simply substitutes every Nothing with the empty list. noCheck allows
-- the type checker to be in the compiler pipeline while under development.
noCheck :: Module (Maybe Signature) -> Module Signature
noCheck (Mod name exported functions) =
  Mod name exported $ map subst functions
    where
      -- subs performs substitution in function definitions.
      subst :: Function (Maybe Signature) -> Function Signature
      subst (Fun ident mt exprs) = Fun ident (Maybe.fromMaybe [] mt) exprs

-- Algorithm W adapted from "Algorithm W Step by Step"

-- awCheck maps the main inference function on the function expressions
-- Build environment delta with top level functions and their respective schemes
-- Type check the function definitions with delta.
-- Fill in missing type signatures.
awCheck :: Module (Maybe Signature) -> TI (Module Signature)
awCheck (Mod name exported functions) = do
  let delta = foldl buildDelta emptyTypeEnv functions
  typed <- mapM (check delta) functions
  return (Mod name exported typed)
    where
      buildDelta :: TypeEnv -> Function (Maybe Signature) -> TypeEnv
      buildDelta = undefined
      check :: TypeEnv -> Function (Maybe Signature) -> TI (Function Signature)
      check e (Fun fname Nothing es) = undefined -- ? 
      check e (Fun fname (Just ts) es) = undefined -- ?

type VarName = String

-- Monotype
data TType
  = TTVar VarName
  | TTInt
  | TTBool
  | TTFun TType TType
    deriving (Eq, Ord)

-- Polytype
data Scheme = Scheme [VarName] TType

-- The members of TTypes can have their free type variables collected and
-- have substitutions applied to them.
class TTypes a where
  -- ftv collects free type variable names
  ftv   :: a -> Set.Set VarName
  -- apply applies a substitution where free type variables are replaced by
  -- an instantiation(?)
  apply :: Subst -> a -> a

instance TTypes TType where
  ftv (TTVar name)  = Set.singleton name
  ftv (TTFun t1 t2) = ftv t1 `Set.union` ftv t2
  ftv _             = Set.empty
  apply s (TTVar name)  = Maybe.fromMaybe (TTVar name) (Map.lookup name s)
  apply s (TTFun t1 t2) = TTFun (apply s t1) (apply s t2)
  apply _ t             = t

instance TTypes a => TTypes [a] where
  ftv = foldr (Set.union . ftv) Set.empty -- why foldr instead of foldl?
  apply s = map (apply s)

instance TTypes Scheme where
  ftv (Scheme vars t)     = ftv t Set.\\ Set.fromList vars
  apply s (Scheme vars t) = Scheme vars (apply (foldr Map.delete s vars) t)
  

-- Substitutions
type Subst = Map.Map VarName TType

nullSubst :: Subst
nullSubst = Map.empty

composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = Map.map (apply s1) s2 `Map.union` s1

-- Type Environments
{-
Maybe use record for TypeEnv?

newtype TypeEnv = TypeEnv {typeEnv :: Map.Map String Scheme}

updateTypeEnv :: (TypeEnv -> TypeEnv) -> TypeEnv -> TypeEnv
updateTypeEnv f env = env { typeEnv = f (typeEnv env) }

updateTypeEnv (Map.delete name) env
updateTypeEnv (Map.map (apply s)) env
-}
newtype TypeEnv = TypeEnv (Map.Map String Scheme)

instance TTypes TypeEnv where
  ftv (TypeEnv e)     = ftv (Map.elems e)
  apply s (TypeEnv e) = TypeEnv (Map.map (apply s) e)

emptyTypeEnv :: TypeEnv
emptyTypeEnv = TypeEnv Map.empty

-- remove
-- removes the binding for type x from environment e
remove :: TypeEnv -> Pattern -> TypeEnv
remove (TypeEnv e) p = case p of
  PVar name -> TypeEnv (Map.delete name e)
  PCon name -> TypeEnv (Map.delete name e)
  _         -> TypeEnv e

-- generalize
generalize :: TypeEnv -> TType -> Scheme
generalize e t = Scheme vars t
  where vars = Set.toList (ftv t Set.\\ ftv e)

-- instantiation
instantiate :: Scheme -> TI TType
instantiate (Scheme vars t) = 
  do  nvars <- mapM (\_ -> newTyVar "'t") vars
      let s = Map.fromList (zip vars nvars)
      return $ apply s t


-- "Fresh names for newly introduced type variables"
data TIEnv = TIEnv{}
data TIState = TIState{ tiSupply :: Int }

-- ErrorT used in the tutorial is deprecated, using ExceptT instead.
-- ExceptT gives us the 
type TI a = ExceptT String (ReaderT TIEnv (StateT TIState IO)) a

runTI :: TI a -> IO (Either String a, TIState)
runTI t = 
  do (res, st) <- runStateT (runReaderT (runExceptT t) initTIEnv) initTIState
     return (res, st)
    where
      initTIEnv   = TIEnv
      initTIState = TIState{ tiSupply = 0 }

newTyVar :: String -> TI TType
newTyVar prefix = 
  do s <- get
     put s{ tiSupply = tiSupply s + 1 }
     return (TTVar (prefix ++ show (tiSupply s)))

-- Unification
-- mgu - most general unifier
mgu :: TType -> TType -> TI Subst
mgu (TTFun t1 t2) (TTFun t3 t4) =
  do s1 <- mgu t1 t3
     s2 <- mgu (apply s1 t2) (apply s1 t4)
     return (s1 `composeSubst` s2)
mgu (TTVar s) t = varBind s t
mgu t (TTVar s) = varBind s t
mgu TTInt TTInt = return nullSubst
mgu TTBool TTBool = return nullSubst
mgu t1 t2 = throwE $ "types do not unify: " ++ show t1 ++ " vs. " ++ show t2

varBind :: VarName -> TType -> TI Subst
varBind s t
  | t == TTVar s         = return nullSubst
  | s `Set.member` ftv t = throwE $ "occurs check fails: " ++ s
                           ++ " vs. " ++ show t
  | otherwise            = return (Map.singleton s t)

-- Main type inference functions

-- tiLit infers types of literals
tiLit :: Literal -> TI (Subst, TType)
tiLit (LS _) = undefined
tiLit (LC _) = undefined
tiLit (LI _) = return (nullSubst, TTInt)
tiLit (LD _) = undefined

-- ti infers the type of expressions
ti :: TypeEnv -> Expression -> TI (Subst, TType) -- Do we need a in expr?
ti (TypeEnv env) (EVar n) =
  case Map.lookup n env of
    Nothing -> throwE $ "unbound variable: " ++ n
    Just sigma -> 
      do t <- instantiate sigma
         return (nullSubst, t)
ti _   (ELit l) = tiLit l
-- awsbs assumes lambda only abstracts over one argument at a time.
-- Hopper allows lambdas which abstract over a list of arguments.
-- Possible adaptions:
-- - Change ast in a prestep to conform to awsbs
-- - Change ti of ELambda to work with the entire list
-- - Change ti of ELambda to work only with the head of the list then
--   solve the rest of the list recursively (recreate the node without head)
-- Current solution is the last of the three
ti env (ELambda [] e)     = ti env e
ti env (ELambda (p:ps) e) =
  do tv <- newTyVar "'t"
     let TypeEnv env' = remove env p
     env'' <- case p of
       PVar name ->
         return $ TypeEnv (env' `Map.union` Map.singleton name (Scheme [] tv))
       PCon name ->
         return $ TypeEnv (env' `Map.union` Map.singleton name (Scheme [] tv))
       PLit _    -> throwE "Cannot abstract over literal"
       PWild     -> throwE "Cannot abstract over wild"
     (s1,t1) <- ti env'' (ELambda ps e) -- under construction
     return (s1, TTFun (apply s1 tv) t1)
ti env exp@(EApp e1 e2) =
  do tv <- newTyVar "a"
     (s1,t1) <- ti env e1
     (s2,t2) <- ti (apply s1 env) e2
     s3 <- mgu (apply s2 t1) (TTFun t2 tv)
     return (s3 `composeSubst` s2 `composeSubst` s1, apply s3 tv)
  `catchE`
  \e -> throwE $ e ++ "\n in " ++ show exp
-- ELet is described in awsbs when it is time to implement ELet

-- typeInference is the main entry point of the typ inferencer.
typeInference :: TypeEnv -> Expression -> TI TType
typeInference env e = do
  (s, t) <- ti env e
  return (apply s t)

-- Pretty printer

instance Show TType where
  showsPrec _ t = shows (prType t)
    where
      prType :: TType -> PP.Doc
      prType (TTVar name)  = PP.text name
      prType TTInt         = PP.text "Int"
      prType TTBool        = PP.text "Bool"
      prType (TTFun t1 t2) = prParenType t1 PP.<+> PP.text "->" PP.<+> prType t2
      prParenType :: TType -> PP.Doc 
      prParenType t' = case t' of
        TTFun _ _ -> PP.parens (prType t')
        _         -> prType t'

instance Show Scheme where
  showsPrec _ s = shows (prScheme s)
    where
      prScheme :: Scheme -> PP.Doc
      prScheme (Scheme vars t) =
        PP.text "forall"
        PP.<+> PP.hcat (PP.punctuate PP.comma (map PP.text vars))
        PP.<> PP.text "." PP.<+> PP.text (show t)

