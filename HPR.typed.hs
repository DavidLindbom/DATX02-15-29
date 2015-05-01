import AST.AST
typed=Mod "HPR" ["HPR.id"] [] [Fun "HPR.id" (TApp (TApp (TCon "Prim.->") (TCon "Prim.Int")) (TCon "Prim.Int")) (ELambda [PVar "HPR.x"] (ECon "HPR.x"))] (fromList [])