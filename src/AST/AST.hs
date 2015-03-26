module AST.AST where

data TCModule = TCModule Name [Name] [(Name,AST)]
  deriving (Eq,Ord,Show,Read)
data RenamedModule = RenamedModule{modId::ModuleId,
                                   exports::[Name],
                                   cons ::[(Name,TypeAST)],
                                   defs::[(Name,AST,Maybe TypeAST)]}
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
  deriving (Eq,Ord,Show,Read)

--renamer should guarantee lambdas only have
--variables and underscores in their pats
data PatAST = VarPat Name
            | WildPat
            | AppPat PatAST PatAST
            | ConPat Name
            | LitPat Lit
            | TuplePat [PatAST]
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


