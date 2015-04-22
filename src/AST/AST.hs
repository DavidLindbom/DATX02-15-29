-- Inspired by "Syntax summary" in "An introduction to Core Erlang"
--
-- Should be somewhere between Haskell syntax and Core Erlang syntax
-- but simple to typecheck
-- https://www.haskell.org/onlinereport/haskell2010/haskellch10.html
-- www.erlang.se/workshop/carlsson.ps 
--
-- The typevariable exists for making it extensible for the typechecker 
-- to add its own data structure

module AST.AST where
import Data.Map

data Module a = Mod Modulename 
                    [Identifier] -- Exports
                    [Identifier] -- Imports
                    [Function a] -- Definitions
                    (Map Identifier Type) -- ADTs
  deriving (Eq,Ord,Show)

type Identifier  = String
type Constructor = String
type Modulename  = String

data Literal = LS String
             | LC Char
             | LI Integer
             | LD Double
             -- | LL []
  deriving (Eq,Ord,Show)

data Function a = Fun Identifier a Expression
  deriving (Eq,Ord,Show)

data Type = TForAll Type
          | TVar Identifier
          | TCon Constructor -- Includes the module name
          | TApp Type Type
  deriving (Eq,Ord,Show)

data Pattern = PVar Identifier
             | PCon Constructor [Pattern]
             | PLit Literal
             | PWild
             | PTuple [Pattern]
  deriving (Eq,Ord,Show)

data Expression = EVar Identifier
                | ECon Constructor
                | ELit Literal
                | ETuple [Expression]
                | ELambda [Pattern] Expression
                | EApp Expression Expression
                | EVal Identifier [Expression]
               -- | EWhere [Function a]
                | ECase Expression [(Pattern, Expression)]
                | ECall Identifier Identifier Expression
               -- | ELet Pattern Expression Expression
  deriving (Eq,Ord,Show)

