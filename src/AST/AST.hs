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

data Function a = Fun Identifier a (Expression a)
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
  deriving (Eq,Ord,Show) -- Should be recursive later for nested lists ect

data Expression a = EVar a Identifier -- TODO: Add EVal for fully applied functions when we have adts
                  | ECon a Constructor
                  | ELit a Literal
                  | ELambda a [Pattern] (Expression a)
                  | EApp a (Expression a) (Expression a)
                 -- | EWhere [(Pattern,Expression)]
                  | ECase a [([Pattern], Expression a)] 
                 -- | ECall a Identifier Identifier [Expression a]
                 -- | ELet Pattern (Expression a) (Expression a)
  deriving (Eq,Ord,Show)
 

