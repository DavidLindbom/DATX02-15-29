module AST.AST where

import Parser.AbsHopper

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

transform :: Module -> ModuleAST (Maybe TypeAST)
transform = undefined

--a -> b ==> ConType "->" [VarType "a",VarType "b"]

expToAST :: Exp -> AST
expToAST e = case e of (EVar (IdVar s)) -> Named s
                       (ECon (IdCon s)) -> Named s
                       (EOpr (IdOpr s)) -> Named s
                       (EString s) -> LitStr s
                       (EChar c) -> LitChar c
                       (EInteger i) -> LitInteger i
                       (EDouble d) -> LitDouble d
                       (EInfix e1 (IdOpr op) e2) -> (Named op) `AppAST` 
                                                    expToAST e1 `AppAST`
                                                    expToAST e2
                       (EApp f x) -> AppAST (expToAST f) $ expToAST x
                       (ELambda ps x) -> LamAST (map patToAST ps) $ expToAST x
patToAST (PVar (IdVar s)) = VarPat s
patToAST PWild = WildPat

