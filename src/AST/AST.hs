module AST.AST where

data ModuleAST a = ModuleAST String [ExportAST] [DefAST a]

data ExportAST = ExportAST String

data DefAST a = DefAST String a AST -- (Maybe TypeAST) AST

data AST = Named String
         | LitStr String
         | LitInteger Integer
         | LitDouble Double
         | LitChar Char
         | LamAST [PatAST] AST
         | AppAST AST AST
data PatAST = VarPat String
            | WildPat

data TypeAST = VarType String
             | ConType String [TypeAST]

--a -> b ==> ConType "->" [VarType "a",VarType "b"]
