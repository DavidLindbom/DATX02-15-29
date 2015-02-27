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

data Function a = Fun Identifier a [Expression a] 
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

data Expression a = EVar a Identifier
                  | ECon a Constructor
                  | ELit a Literal
                  | ELambda a [Pattern] (Expression a)
                  | EApp a (Expression a) (Expression a)
  deriving (Eq,Ord,Show)
 


{- 
 - module MyModule (a,b) where
 - a :: Int -> b -> Bool -> Int
 - a = \_ _ True -> 0
 - a = \n _ False -> n
 -
 - b = a 4 'c' False
 -}

{-

untyped :: Module (Maybe Signature)
untyped = Mod "MyModule" e f
  where e = ["a","b"]
        f = [Fun "a" (Just [TName "Int" [], TVar "b", TName "Bool" [], TName "Int" []]) [a1,a2]
            ,Fun "b" Nothing [b1]
            ]
        a1 = ELambda Nothing [PWild, PWild, PCon "True"] (ELit Nothing (LI 0))
        a2 = ELambda Nothing [PVar "n", PWild, PWild] (EVar Nothing "n")
        b1 = EApp Nothing (EApp Nothing (EApp Nothing (EVar Nothing "a") (ELit Nothing (LI 4))) 
                                        (ELit Nothing (LC 'c')))
                          (ECon Nothing "False")

-- Run some typecheck function: Module (Maybe Signature) -> Err (Module Signature)

typed :: Module Signature
typed = Mod "MyModule" e f
  where e = ["a", "b"]
        f = [Fun "a" [TName "Int" [], TVar "a", TName "Bool" [], TName "Int" []] [a1,a2]
            ,Fun "b" [TName "Int" []] [b1]
            ]
        a1 = ELambda [TName "Int" [], TVar "a", TName "Int" []] 
                     [PLit (LI 1), PWild] (ELit [TName "Int" []] (LI 0))
        a2 = ELambda [TName "Int" [], TVar "a", TName "Int" []] 
                     [PVar "n", PWild] (EVar [TName "Int" []] "n")
        b1 = EApp [TName "Int" []] 
                  (EApp [TVar "a", TName "Int" []] (EVar [TName "Int" [], TVar "a", TName "Int" []] "a") (ELit [TName "Int" []] (LI 4)))
                  (ELit [TName "Char" []] (LC 'c'))

--}
