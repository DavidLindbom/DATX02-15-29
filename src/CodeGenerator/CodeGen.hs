

module CodeGenerator.CodeGen where
import AST.AST
import TypeChecker.TC

import Language.CoreErlang.Syntax
import qualified Control.Monad.Reader as R
import qualified Data.Set as S
import Data.Char (isAlpha,ord)

--Todo AbsHopper.Module -> Module with Optional types
{-
AST.Module: Maybe Type, Name, AST
Typechecked: (InterfaceFile,Name,[(Name,AST)])

-}
codeGen :: TCModule -> Module
codeGen (TCModule (Name _ s) exports defs) = 
    Module 
        (Atom s) 
        (map (__fun . (\(Name _ s) -> s)) exports)
        [] $
        (map (\((Name _ s),ast,_) -> FunDef (Constr $ __fun s) $ 
                                   Constr $ Lambda [] $ exps $
                                          apply 
                                          (fun "curry" "curry" 1)
                                          (R.runReader 
                                           (astToCore ast) S.empty))) 
        defs
            where
              apply ef ex =  App (exps ef) [exps ex]
              fun mod name arity = modCall 
                                   (atom "erlang")
                                   (atom "make_fun")
                                   [atom mod,atom name,Lit$LInt arity]
--(Lambda[]$ 
--apply (fun "curry" "curry" 1) $
--cerl distinguishes between named functions and variables
--three cases: local named function;
--             named function from other module;
--             variable
--The reader value contains all names that are defined
--in a let or lambda
astToCore :: AST -> R.Reader (S.Set Name) Exp
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
astToCore (Named n@(Name _ s)) = do isAVariable <- R.asks (S.member n)
                                    return $ if isAVariable
                                             then Var $ handleVarName n 
                                             else Fun $ __fun s
astToCore (AppAST f x) = do fc <- astToCore f
                            xc <- astToCore x
                            return $ App (exps fc) [exps xc]
astToCore (LitAST astLit) = return $ Lit $ astLitToCoreLit astLit
astToCore (LamAST ps body) = do let varNames = map (\(VarPat n)-> n) ps
                                bodyc <- R.local 
                                         (inserts varNames)$
                                         astToCore body
                                return $ Lambda (map 
                                                 handleVarName
                                                 varNames) $ exps bodyc
astToCore (CaseAST x clauses) = do exp <- astToCore x
                                   alts <- mapM (\(pat,res)->
                                                 do 
                                                   let cpat = patToCore pat
                                                       vns = namesOccP pat
                                                   cres <- R.local 
                                                           (inserts vns) $
                                                        astToCore res
                                                   return $ Constr $ Alt
                                                       (pats cpat)
                                                       (Guard$
                                                        exps$
                                                        atom"true")
                                                       (exps cres)) clauses
                                   return $ Case (exps exp) alts
astToCore (TupleAST asts) = do cores <- mapM astToCore asts
                               return $ Tuple $ map exps cores
astToCore ast = error $ "Nonexhaustive: " ++ show ast

patToCore :: PatAST -> Pat
patToCore (VarPat n) = PVar $ handleVarName n
patToCore (LitPat l) = PLit $ astLitToCoreLit l
patToCore (TuplePat ps) = PTuple $ map patToCore ps
-- x:xs ==> [X|Xs]
patToCore (AppPat 
           (AppPat 
            (ConPat (Name (Just["Prim"]) ":"))
            x)
           xs) = PList (LL [patToCore x] $ patToCore xs)
patToCore (AsPat (VarPat n) p) = PAlias (Alias (handleVarName n) $ patToCore p)
--Oh no! Core Erlang has no wildcard pattern!
--Problem with making _ ==> Wild@Pat:
--    what about receive? what about \_ _ -> bla?
--Refactoring to make it possible to generate unique variable names
--will be necessary.
patToCore WildPat = PVar "Wild@Pat"

inserts ns set = foldr (\n set -> S.insert n set) set ns

handleVarName (Name Nothing s) = 
    if isPrefixName s
    then 'P':s
    else 'I':
         encodeInfix s         
        where
          isPrefixName (c:s) = c == '_' || isAlpha c
        --converts an infix variable name such as "+" to
        --a representation Erlang is guaranteed to accept.
        --Does this in a very simple fashion (by turning
        --characters into their integer representation).
          encodeInfix cs = foldr1 (\s str -> s ++ "_" ++ str) $ 
                           map (show.ord) cs
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