module AST.Liam_TC_AST where

import Data.IORef

data TCModule = TCModule Name [Name] [(Name,AST,TypeAST)]
  --deriving (Eq,Ord,Show)
data RenamedModule = RenamedModule{modId::ModuleId,
                                   exports::[Name],
                                   cons ::[(Name,TypeAST)],
				   imports :: [(Name,TypeAST)],
                                   defs::[(Name,AST,Maybe TypeAST)]}
                     --deriving (Eq,Ord,Show)
--Module data: name, exports, definitions, types?
--TODO instances
--UntypedModule
--TypedModule?No, InterfaceFile

--Name now uses defunct moduleid field to implement typeclasses!
data Name = Name (IORef (Either String TypeAST)) String
instance Ord Name where
    compare (Name _ s) (Name _ s2) = compare s s2
instance Eq Name where
    (Name _ s) == (Name _ s2) = s == s2
--type ModuleId = [String]
type ModuleId = String
instance Show Name where
    show (Name _ s) = s

data AST = Named Name
         | LitAST Lit
         | LamAST [PatAST] AST
         | AppAST AST AST
         | CaseAST AST [(PatAST,AST)]
         | IfAST AST AST AST
         | TupleAST [AST]
         | WildAST --used in type checker
         | AsAST AST AST --used in type checker.
           --I'm beginning to regret converting patterns
           --to exprs to typecheck them.
         | UnsafeReceive [(PatAST,AST)] (Timeout,AST)
         | Receive [(IORef TypeAST,PatAST,AST)] (Timeout,AST)
  --deriving (Eq,Ord,Show)
data Timeout = Infinity | Timeout Integer deriving (Eq,Ord,Show)

--renamer should guarantee lambdas only have
--variables and underscores in their pats
data PatAST = VarPat Name
            | WildPat
            | AppPat PatAST PatAST
            | ConPat Name
            | LitPat Lit
            | TuplePat [PatAST]
            | AsPat 
              PatAST --always a variable
              PatAST
  deriving (Eq,Ord,Show)

type TyVarName = Name
tyVar :: String -> TypeAST
tyVar = VarT . Name (error "IORef in Type variable should never be inspected!")
data TypeAST = ForallT TypeAST 
             --a -> forall a . a
             --may refactor later
             | Implicit TypeAST TypeAST
             --e.g. show :: Implicit t => t -> String
             --     show (ConT "Prim.Number") s = ...
             --     typeOf :: Implicit t => t -> Type
             --     typeOf t _ = t
             | VarT TyVarName
             | ConT Name
             | AppT TypeAST TypeAST
    deriving (Eq,Ord,Show)

data Lit = StringL String
         | IntegerL Integer
         | DoubleL Double
         | CharL Char
         | AtomL String deriving (Eq,Ord,Show)

--a -> Int = AppT (ConT (Name (Just ["Prim"]) "->")
--           VarT (Name Nothing "a") `AppT` (Name Nothing "b")

