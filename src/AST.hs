-- Inspired by "Syntax summary" in "An introduction to Core Erlang"
module AST where

data Module = Mod [Identifier] [Function]
  deriving (Show)

-- Function name
data Identifier = Id String
  deriving (Show) 
  
data Constructor = Con String
  deriving (Show)

data Literal = LS String
             | LC Char
             | LI Integer
             | LD Double
             -- | LL []
  deriving (Show)

data Function = Fun Identifier Signature [([Pattern],Expression)]
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

