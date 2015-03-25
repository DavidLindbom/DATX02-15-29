

module CodeGenerator.CodeGen where
import AST.AST

import Language.CoreErlang.Syntax
import qualified Control.Monad.Reader as R
import qualified Data.Set as S
import Data.Char (isAlpha,ord)

--Todo AbsHopper.Module -> Module with Optional types
{-
AST.Module: Maybe Type, Name, AST
Typechecked: (InterfaceFile,Name,[(Name,AST)])

-}
moduleASTToModule :: TCModule -> Module
moduleASTToModule (TCModule s exports defs) = 
    Module 
        (Atom s) 
        (map __fun exports)
        []
        (map (\(s,ast) -> FunDef (Constr $ __fun s) $ Constr $ R.runReader 
                          (astToCore ast) S.empty) defs)

--cerl distinguishes between named functions and variables
--three cases: local named function;
--             named function from other module;
--             variable
--The reader value contains all names that are defined
--in a let or lambda
astToCore :: AST -> R.Reader (S.Set String) Exp
--no variables have module ids!
--how to handle Package.Module? TODO ask Johan
--m1.m2.m3 => 'm1.m2.m3'
astToCore (Named (Name (Just mid) s)) = return $ modCall 
                                        (atom "erlang")
                                        (atom "make_fun")
                                        [moduleAtom,
                                         atom $ "__"++s,
                                         Lit $ LInt 0]
    where
      moduleAtom = atom $ foldr1 (\s atname -> s++"."++atname) mid
--Two cases: variable or named value
--TODO ask Johan what names are permitted for variables
astToCore (Named (Name Nothing s)) = do isAVariable <- R.asks (S.member s)
                                        return $ if isAVariable
                                                 then if isPrefixName s
                                                         then Var$'P':s
                                                         else Var$'I':
                                                              encodeInfix s
                                                 else Fun $ __fun s
    where
      isPrefixName (c:s) = c == '_' || isAlpha c
      --converts an infix variable name such as "+" to
      --a representation Erlang is guaranteed to accept.
      --Does this in a very simple fashion (by turning
      --characters into their integer representation).
      encodeInfix cs = foldr1 (\s str -> s ++ "_" ++ str) $ map (show.ord) cs
astToCore (AppAST f x) = do fc <- astToCore f
                            xc <- astToCore x
                            return $ App (exps fc) [exps xc]
astToCore (LitAST astLit) = return $ Lit $ astLitToCoreLit astLit
--
astToCore (LamAST ps body) = do let varStrs = (map (\(VarPat (Name Nothing s))
                                                    -> s) ps)
                                bodyc <- R.local 
                                         (inserts varStrs)$
                                         astToCore body
                                return $ Lambda varStrs $ exps bodyc
    where
      inserts strs set = foldr (\str set -> S.insert str set) set strs
astLitToCoreLit :: Lit -> Literal
astLitToCoreLit (StringL s) = LString s
astLitToCoreLit (IntegerL i) = LInt i
astLitToCoreLit (DoubleL d) = LFloat d
astLitToCoreLit (CharL c) = LChar c
astLitToCoreLit (AtomL s) = LAtom $ Atom s

exps :: Exp -> Exps
exps = Exp . Constr
pats :: Pat -> Pats
pats = Pat
modCall :: Exp -> Exp -> [Exp] -> Exp
modCall mod fn args = ModCall (exps mod,exps fn) $ map exps args

atom = Lit . LAtom . Atom 
__fun s = Function (Atom$"__"++s,0)