
module TypeChecker.Convert where

--A module containing functions to convert
--to and from the representation of modules that
--the liam-tc-typechecker uses.
--Since grammar does not yet have modules,
--I won't implement "M1.M2.foo" ==> Name ["M1","M2"] foo;
--I will assume there are no module prefixes.
import qualified AST.Liam_TC_AST as L
import qualified AST.AST as A
import Data.Char (isUpper)
import Control.Arrow ((***))
import Data.Map (toList, empty)

moduleToRenamed :: A.Module (Maybe A.Type)-> L.RenamedModule
moduleToRenamed (A.Mod s exps _ fs adts) = L.RenamedModule {
                                	       L.modId = [s],
                                	       L.exports = map idToName exps,
                                	       L.cons = (map (idToName ***
					       	     typeToTypeAST) $ toList adts),
					       L.imports = [],
                                	       L.defs = (map functionToName'AST'MType fs)}
    where
      splitEveryDot s = case break (=='.') s of
                          (s,"") -> [s]
                          (s,'.':ss) -> s:splitEveryDot ss

idToName :: A.Identifier -> L.Name
idToName s = L.Name Nothing s

functionToName'AST'MType :: A.Function (Maybe A.Type) -> 
    (L.Name,L.AST,Maybe L.TypeAST)
functionToName'AST'MType (A.Fun id msig expression) = 
    (idToName id,expToAST expression, fmap typeToTypeAST msig)

typeToTypeAST :: A.Type -> L.TypeAST
--typeToTypeAST (A.TName s ts) = foldl1 L.AppT $ (L.ConT $ idToName s):
--                               map typeToTypeAST ts
typeToTypeAST (A.TVar s) = L.VarT $ idToName s
--typeToTypeAST (A.TFun ts) = foldr1 arrowt $ map typeToTypeAST ts
--    where
--      arrowt a b = L.ConT(L.Name(Just["Prim"])"->") `L.AppT` a `L.AppT` b
typeToTypeAST (A.TCon s) = L.ConT $ idToName s
typeToTypeAST (A.TApp tf tx) = L.AppT (typeToTypeAST tf) (typeToTypeAST tx)

expToAST (A.EVar id) = L.Named $ idToName id
expToAST (A.ECon id) = L.Named $ idToName id
expToAST (A.ELit lit) = L.LitAST $ aLitToLLit lit
expToAST (A.ETuple exps) = L.TupleAST $ map expToAST exps
expToAST (A.ELambda ps e) = L.LamAST (map patToPatAST ps) $ expToAST e
expToAST (A.EApp f x) = L.AppAST (expToAST f) (expToAST x)
expToAST (A.ECase exp clauses) = L.CaseAST (expToAST exp) $
                                 map (\(pat,exp) ->
                                          (patToPatAST pat,expToAST exp))
                                 clauses

patToPatAST :: A.Pattern -> L.PatAST
patToPatAST (A.PVar id) = L.VarPat $ idToName id
patToPatAST (A.PCon id ps) = foldl L.AppPat (L.ConPat (idToName id)) $ map patToPatAST ps
patToPatAST (A.PLit lit) = L.LitPat $ aLitToLLit lit
patToPatAST A.PWild = L.WildPat
patToPatAST (A.PTuple ps) = L.TuplePat $ map patToPatAST ps

aLitToLLit :: A.Literal -> L.Lit
aLitToLLit (A.LS s) = L.StringL s
aLitToLLit (A.LC c) = L.CharL c
aLitToLLit (A.LI i) = L.IntegerL i
aLitToLLit (A.LD d) = L.DoubleL d

--------------------------------------------------------------------------------
--And now in the other direction...
--------------------------------------------------------------------------------

--Are signatures just singleton lists?

tcModToModule :: L.TCModule -> A.Module A.Type
tcModToModule (L.TCModule n ns ns'asts'types) = 
    A.Mod 
    (nameToId n)
    (map nameToId ns) 
    [{-NOTE: We import nothing here-}] 
    (map toFunction ns'asts'types)
    (empty{-NOTE: We don't give any ADT types-})
nameToId (L.Name _ s) = s
toFunction (n,ast,typ) = A.Fun (nameToId n) (typeASTToType typ) (astToExp ast)

--note: it should support partially applied (->)
{-typeASTToType ((L.ConT (L.Name (Just["Prim"])"->")) `L.AppT` a `L.AppT` b) = 
    A.TFun $ map typeASTToType $ a:args b
     where 
       args (L.ConT (L.Name (Just["Prim"])"->") `L.AppT` a `L.AppT` b) = 
           a:args b
       args t = [t]
typeASTToType app@(L.AppT con t) = A.TName (nm con) (map typeASTToType $ 
                                                    args app)
    where
      args (L.AppT con t) = args con ++ [t]
      args t = [t]
      nm (L.AppT con t) = nm con
      nm (L.ConT (L.Name _ s)) = s-}
typeASTToType (L.ConT n) = A.TCon $ show n
typeASTToType (L.VarT n) = A.TVar $ show n
typeASTToType (L.ForallT t) = A.TForAll $ typeASTToType t


astToExp :: L.AST -> A.Expression
astToExp (L.Named (L.Name _ (c:s))) 
    | c == ':' || isUpper c = A.ECon (c:s)
    | otherwise = A.EVar (c:s)
astToExp (L.LitAST lit) = A.ELit $ lLitToALit lit
astToExp (L.TupleAST exps) = A.ETuple $ map astToExp exps
astToExp (L.LamAST ps body) = A.ELambda (map patASTToPat ps) (astToExp body)
astToExp (L.AppAST f x) = A.EApp (astToExp f) (astToExp x)
astToExp (L.CaseAST ast clauses) = A.ECase (astToExp ast) $ map
                                   (\(pat,res)->(patASTToPat pat,astToExp res))
                                   clauses

patASTToPat :: L.PatAST -> A.Pattern
patASTToPat (L.VarPat (L.Name _ s)) = A.PVar s
patASTToPat app@(L.AppPat _ _) = unfoldApps app []
	where 
		unfoldApps (L.ConPat (L.Name _ s)) ps = 
			A.PCon s (reverse $ map patASTToPat ps)
		unfoldApps (L.AppPat app p) ps =
			unfoldApps app (p:ps)
--patASTToPat (L.ConPat (L.Name _ s)) = A.PCon s
patASTToPat (L.LitPat lit) = A.PLit $ lLitToALit lit
patASTToPat L.WildPat = A.PWild
patASTToPat (L.TuplePat ps) = A.PTuple $ map patASTToPat ps

lLitToALit (L.StringL s) = A.LS s
lLitToALit (L.CharL c) = A.LC c
lLitToALit (L.IntegerL i) = A.LI i
lLitToALit (L.DoubleL d) = A.LD d
