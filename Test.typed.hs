import AST.AST
typed=Mod "Test" ["Test.exp","Test.f","Test.ignore","Test.impl","Test.urcv","Test.F","Test.Left","Test.Right","Test.T"] [] [Fun "Test.F" (TForAll (TCon "Test.B")) (ELit (LA "f")),Fun "Test.Left" (TForAll (TApp (TApp (TCon "Prim.->") (TCon "Test.B")) (TCon "Test.Either"))) (ELambda [PVar "x1"] (ETuple [ELit (LA "left"),EVar "x1"])),Fun "Test.Right" (TForAll (TApp (TApp (TCon "Prim.->") (TCon "Prim.Number")) (TCon "Test.Either"))) (ELambda [PVar "x1"] (ETuple [ELit (LA "right"),EVar "x1"])),Fun "Test.T" (TForAll (TCon "Test.B")) (ELit (LA "t")),Fun "Test.exp" (TCon "Prim.Number") (ECase (EApp (EVar "Test.Left") (EVar "Test.T")) [(PCon "Test.Left" [PVar "x"],EApp (EVar "Test.ignore") (EVar "x")),(PCon "Test.Right" [PVar "x"],EApp (EVar "Test.ignore") (EVar "x"))]),Fun "Test.f" (TApp (TApp (TCon "Prim.->") (TCon "Test.B")) (TCon "Test.B")) (ELambda [PVar "_arg1"] (ECase (ETuple [EVar "_arg1"]) [(PTuple [PCon "Test.T" []],EVar "Test.T")])),Fun "Test.ignore" (TForAll (TApp (TApp (TCon "Prim.->") (TVar "t2")) (TCon "Prim.Number"))) (ELambda [PVar "_arg1"] (ECase (ETuple [EVar "_arg1"]) [(PTuple [PWild],ELit (LI 0))])),Fun "Test.impl" (TForAll (TImplicit (TVar "t") (TVar "t"))) (EVar "Test.impl"),Fun "Test.urcv" (TApp (TCon "Prelude.IO") (TCon "Prim.String")) (EReceive [(PTuple [PCon "Prelude.ConT" [PLit (LS "Prim.Number")],PLit (LI 3)],ELit (LS "yoop"))] (Infinity,ELit (LS "nay man")))] (fromList [])