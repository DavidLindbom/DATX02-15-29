import AST.AST
typed=Mod "HPR" ["HPR.id"] [] [Fun "HPR.id" (TForAll (TApp (TApp (TCon "Prim.->") (TVar "t1")) (TVar "t1"))) (ELambda [PVar "x"] (EVar "x")),Fun "HPR.loop" (TForAll (TVar "t0")) (EVar "HPR.loop")] (fromList [])