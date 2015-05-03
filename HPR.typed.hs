import AST.AST
typed=Mod "HPR" ["HPR.id"] [] [Fun "HPR.loop" (TForAll (TApp (TApp (TCon "Prim.->") (TVar "t2")) (TVar "t3"))) (ELambda [PVar "x"] (EApp (EVar "HPR.loop") (EVar "x")))] (fromList [])