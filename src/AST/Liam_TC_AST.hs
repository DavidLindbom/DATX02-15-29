module AST.Liam_TC_AST where

data TCModule = TCModule Name [Name] [(Name,AST,TypeAST)]
  deriving (Eq,Ord,Show,Read)
data RenamedModule = RenamedModule{modId::ModuleId,
                                   exports::[Name],
                                   cons ::[(Name,TypeAST)],
				   imports :: [(Name,TypeAST)],
                                   defs::[(Name,AST,Maybe TypeAST)]}
                     deriving (Eq,Ord,Show,Read)
--Module data: name, exports, definitions, types?
--TODO instances
--UntypedModule
--TypedModule?No, InterfaceFile

data Name = Name (Maybe ModuleId) String deriving (Eq,Ord,Read)
type ModuleId = [String]
instance Show Name where
    show (Name Nothing s) = s
    show (Name (Just ms) s) = (ms >>= (++".")) ++ s

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
  deriving (Eq,Ord,Show,Read)

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
  deriving (Eq,Ord,Show,Read)

type TyVarName = Name
tyVar :: String -> TypeAST
tyVar = VarT . Name Nothing
data TypeAST = ForallT TypeAST 
             --a -> forall a . a
             --may refactor later
             | VarT TyVarName
             | ConT Name
             | AppT TypeAST TypeAST
    deriving (Eq,Ord,Show,Read)

data Lit = StringL String
         | IntegerL Integer
         | DoubleL Double
         | CharL Char
         | AtomL String deriving (Eq,Ord,Show,Read)

--a -> Int = AppT (ConT (Name (Just ["Prim"]) "->")
--           VarT (Name Nothing "a") `AppT` (Name Nothing "b")

