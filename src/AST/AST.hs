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

data Module a = Mod String [Identifier] [Function a]
  deriving (Eq,Ord,Show)

type Identifier  = String
type Constructor = String

data Literal = LS String
             | LC Char
             | LI Integer
             | LD Double
             -- | LL []
  deriving (Eq,Ord,Show)

data Function a = Fun Identifier a Expression
  deriving (Eq,Ord,Show) -- Function arguments is desugared to lambdas

type Signature = [Type]

data Type = TName String [Type]
          | TVar  String
          | TFun  [Type] -- For functions as arguments
  deriving (Eq,Ord,Show)

data Pattern = PVar Identifier
             | PCon Constructor
             | PLit Literal
             | PWild
             | PTuple [Pattern]
  deriving (Eq,Ord,Show) -- Should be recursive later for nested lists ect

-- Removed type parameter from expression. Type checker got too confused.
data Expression = EVar Identifier -- TODO: Add EVal for fully applied functions when we have adts
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
 

