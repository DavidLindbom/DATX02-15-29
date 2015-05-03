import AST.AST
typed=Mod "HPR" ["HPR.id"] [] [Fun "HPR.id" (TForAll (TApp (TApp (TCon "Prim.->") (TVar "t1")) (TVar "t1"))) (ELambda [PVar "HPR.x"] (EVar "HPR.x"))] (fromList [])