import AST.AST
ast=Mod "HPR" ["HPR.id"] [] [Fun "HPR.id" (Just (TApp (TApp (TCon "Prim.->") (TCon "Prim.Int")) (TCon "Prim.Int"))) (ELambda [PVar "HPR.x"] (EVar "HPR.x"))] (fromList [])