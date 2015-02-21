-- Inspired by "Syntax summary" in "An introduction to Core Erlang"
--
-- Should be somewhere between Haskell syntax and Core Erlang syntax
-- but simple to typecheck
-- https://www.haskell.org/onlinereport/haskell2010/haskellch10.html
-- www.erlang.se/workshop/carlsson.ps 
module AST.AST where

data Module a = Mod String [Identifier] [Function a]
  deriving (Show)

type Identifier  = String
type Constructor = String

data Literal = LS String
             | LC Char
             | LI Integer
             | LD Double
             -- | LL []
  deriving (Show)

data Function a = Fun Identifier a [Expression a] -- Function arguments is desugared to lambdas
                | Unc Identifier a [Expression a] -- Uncurried functions, just for code generation
  deriving (Show)

type Signature = [Type]

data Type = TName String -- [Type]
          | TVar  String -- [Type]
  deriving (Show) -- Should be recursive later for parametrized algebraic data types?

data Pattern = PVar Identifier
             | PCon Constructor
             | PLit Literal
             | PWild  
  deriving (Show) -- Should be recursive later for nested lists ect

data Expression a = EVar a Identifier
                  | ECon a Constructor
                  | ELit a Literal
                  | ELambda a [Pattern] (Expression a)
                  | EApp a (Expression a) (Expression a)
  deriving (Show) 


{- 
 - module MyModule (a,b) where
 - a :: Int -> b -> Int
 - a = \1 _ -> 0
 - a = \n _ -> n
 -
 - b = a 4 'c'
 -}

untyped :: Module (Maybe Signature)
untyped = Mod "MyModule" e f
  where e = ["a","b"]
        f = [Fun "a" (Just [TName "Int", TVar "b", TName "Int"]) [a1,a2]
            ,Fun "b" Nothing [b1]
            ]
        a1 = ELambda Nothing [PLit (LI 1), PWild] (ELit Nothing (LI 0))
        a2 = ELambda Nothing [PVar "n", PWild] (EVar Nothing "n")
        b1 = EApp Nothing (EApp Nothing (EVar Nothing "a") (ELit Nothing (LI 4))) (ELit Nothing (LC 'c'))

-- Run some typecheck function: Module (Maybe Signature) -> Err (Module Signature)

typed :: Module Signature
typed = Mod "MyModule" e f
  where e = ["a", "b"]
        f = [Fun "a" [TName "Int", TVar "b", TName "Int"] [a1,a2]
            ,Fun "b" [TName "Int"] [b1]
            ]
        a1 = ELambda [TName "Int", TVar "a", TName "Int"] 
                     [PLit (LI 1), PWild] (ELit [TName "Int"] (LI 0))
        a2 = ELambda [TName "Int", TVar "a", TName "Int"] 
                     [PVar "n", PWild] (EVar [TName "Int"] "n")
        b1 = EApp [TName "Int"] 
                  (EApp [TVar "a", TName "Int"] (EVar [TName "Int", TVar "a", TName "Int"] "a") (ELit [TName "Int"] (LI 4)))
                  (ELit [TName "Char"] (LC 'c'))


