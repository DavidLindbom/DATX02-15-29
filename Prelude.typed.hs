import AST.AST
typed=Mod "Prelude" ["Prelude.another_sig"] [] [Fun "Prelude.another_sig" (TCon "Prim.Number") (ELit (LI 3))] (fromList [])