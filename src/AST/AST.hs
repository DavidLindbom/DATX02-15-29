-- Inspired by "Syntax summary" in "An introduction to Core Erlang"
--
-- Should be somewhere between Haskell syntax and Core Erlang syntax
-- but simple to typecheck
-- https://www.haskell.org/onlinereport/haskell2010/haskellch10.html
-- www.erlang.se/workshop/carlsson.ps 
module AST.AST where

data Module = Mod String [Identifier] [Function]
  deriving (Show)

newtype Identifier  = Id String  deriving (Show) -- Function name, variable...
newtype Constructor = Con String deriving (Show)

data Literal = LS String
             | LC Char
             | LI Integer
             | LD Double
             -- | LL []
  deriving (Show)

data Function = Fun Identifier Signature [([Pattern],Expression)]
              | Unc Identifier Signature [([Pattern],Expression)] -- Uncurried
  deriving (Show)

data Signature = Sig [Type]
  deriving (Show)

data Type = TName String -- [Type]
          | TVar  String -- [Type]
  deriving (Show) -- Should be recursive later for parametrized algebraic data types?

data Pattern = PVar Identifier
             | PCon Constructor
             | PLit Literal
             | PWild  
  deriving (Show) -- Should be recursive later for nested lists ect

data Expression = EVar Identifier
                | ECon Identifier
                | ELit Literal
                | ELambda [Pattern] Expression
                | EApp Expression Expression
  deriving (Show) 

