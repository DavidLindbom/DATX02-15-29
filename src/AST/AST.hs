module AST.AST where

data ModuleAST a = ModuleAST String [ExportAST] [DefAST a]
  deriving (Eq,Ord,Show,Read)

data ExportAST = ExportAST String
  deriving (Eq,Ord,Show,Read)

data DefAST a = DefAST String a AST -- (Maybe TypeAST) AST
  deriving (Eq,Ord,Show,Read)

data AST = Named String
         | LitStr String
         | LitInteger Integer
         | LitDouble Double
         | LitChar Char
         | LamAST [PatAST] AST
         | AppAST AST AST
  deriving (Eq,Ord,Show,Read)
data PatAST = VarPat String
            | WildPat
  deriving (Eq,Ord,Show,Read)

data TypeAST = VarType String
             | ConType String [TypeAST]
  deriving (Eq,Ord,Show,Read)

--a -> b ==> ConType "->" [VarType "a",VarType "b"]
