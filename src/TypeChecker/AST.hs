
module AST where

data AST = Named String
         | LitStr String
         | LitInteger String
         | LitDouble Double
         | LitChar Char
         | LamAST [PatAST] AST
         | AppAST AST AST
data PatAST = VarPat String
            | WildPat
data TypeAST = VarType String
             | ConType String [TypeAST]
--a -> b ==> ConType "->" [VarType "a",VarType "b"]

typeChecker :: [(String,AST)] -> Maybe [(String,TypeAST)]
typeChecker = undefined
