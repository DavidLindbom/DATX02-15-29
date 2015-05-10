
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

import System.IO.Unsafe (unsafePerformIO)
import Data.IORef
import Data.List (isPrefixOf)

moduleToRenamed :: A.Module (Maybe A.Type)-> L.RenamedModule
moduleToRenamed (A.Mod s exps _ fs adts) = L.RenamedModule {
                                	       L.modId = s,
                                	       L.exports = map idToName exps,
                                	       L.cons = (map (idToName ***
					       	     typeToTypeAST) $ 
                                                         toList adts),
					       L.imports = [],
                                	       L.defs = (map 
                                                   functionToName'AST'MType fs)}

idToName :: A.Identifier -> L.Name
idToName s = L.Name (unsafePerformIO $ newIORef $ Left s) s

functionToName'AST'MType :: A.Function (Maybe A.Type) -> 
    (L.Name,L.AST,Maybe L.TypeAST)
functionToName'AST'MType (A.Fun id msig expression) = 
    (idToName id,expToAST expression, fmap typeToTypeAST msig)

typeToTypeAST :: A.Type -> L.TypeAST
--yet another syntax hack:
-- Implicit t1 -> t2 ==> Implicit t1 t2
typeToTypeAST (A.TCon "Prim.->" `A.TApp` 
               (A.TCon implicit `A.TApp` t1) `A.TApp`
               t2) | ".Implicit" `isSuffixOf` implicit =
 L.Implicit (typeToTypeAST t1) (typeToTypeAST t2)
typeToTypeAST (A.TForAll t) = L.ForallT $ typeToTypeAST t
typeToTypeAST (A.TVar s) = L.VarT $ idToName s
typeToTypeAST (A.TCon s) = L.ConT $ idToName s
typeToTypeAST (A.TApp tf tx) = L.AppT (typeToTypeAST tf) (typeToTypeAST tx)

--Abusing syntax to create receive (typesafe) & unsafeReceive (non-typesafe,
-- but more like Erlang's receive):
expToAST (A.ECase (A.EVar unsafeReceive) cases) 
    | isSuffixOf ".unsafeReceive" unsafeReceive = 
        L.UnsafeReceive (map (\(pat,exp)->(patToPatAST pat,expToAST exp)) $
                             init cases)
        $ patToTimeout $ last cases
expToAST (A.ECase (A.EVar receive) cases)
    | isSuffixOf ".receive" receive =
        L.Receive (map (\(pat,exp)->(unsafePerformIO$newIORef $ 
                                                    error 
                                     "IORef in receive not assigned!",
                                     patToPatAST pat,
                                     expToAST exp)) $ init cases)
      $ patToTimeout $ last cases
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
isSuffixOf suffix str = reverse suffix `isPrefixOf` reverse str
patToTimeout (A.PCon after [timeout],exp) 
    | isSuffixOf ".After" after = (,)
        (case timeout of
           A.PCon infinity [] | isSuffixOf ".Infinity" infinity ->
                                  L.Infinity
           A.PCon timeout [A.PLit (A.LI i)] | isSuffixOf ".Timeout" timeout ->
                                                L.Timeout i)
        (expToAST exp)

patToPatAST :: A.Pattern -> L.PatAST
patToPatAST (A.PVar id) = L.VarPat $ idToName id
patToPatAST (A.PCon id ps) = foldl L.AppPat (L.ConPat (idToName id)) $ 
                             map patToPatAST ps
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
typeASTToType (L.Implicit timpl t) = A.TImplicit 
                                     (typeASTToType timpl)
                                     (typeASTToType t)
typeASTToType (L.AppT tf tx) = A.TApp 
                               (typeASTToType tf)
                               (typeASTToType tx)

astToExp :: L.AST -> A.Expression
astToExp (L.UnsafeReceive cases (timeout,timeoutAST)) = 
    A.EReceive (map (\(pat,ast) -> (patASTToPat pat,astToExp ast)) cases)
     $ (,) (case timeout of
              L.Infinity -> A.Infinity
              L.Timeout i -> A.Timeout i)
     $ astToExp timeoutAST
astToExp (L.Receive iorefs'cases (timeout,timeoutAST)) =
    A.EReceive (map (\(ioref,pat,ast) -> (patASTToPat $ L.TuplePat 
                                          [L.LitPat $ L.AtomL "t2",
                                            typeToPat $ 
                                                       unsafePerformIO $
                                                         readIORef ioref,
                                           pat],
                                                      astToExp ast)) 
                iorefs'cases)
     $ (,) (case timeout of
              L.Infinity -> A.Infinity
              L.Timeout i -> A.Timeout i)
     $ astToExp timeoutAST
astToExp (L.Named (L.Name ioref s)) = 
    case unsafePerformIO $ readIORef ioref of
          Left _ -> A.EVar s
          Right implicitTypeArg -> A.EApp (A.EVar s) (astToExp $ 
                                                     typeToAST implicitTypeArg)
astToExp (L.LitAST lit) = A.ELit $ lLitToALit lit
astToExp (L.TupleAST exps) = A.ETuple $ map astToExp exps
astToExp (L.LamAST ps body) = A.ELambda (map patASTToPat ps) (astToExp body)
astToExp (L.AppAST f x) = A.EApp (astToExp f) (astToExp x)
astToExp (L.CaseAST ast clauses) = A.ECase (astToExp ast) $ map
                                   (\(pat,res)->(patASTToPat pat,astToExp res))
                                   clauses

typeToPat :: L.TypeAST -> L.PatAST
typeToPat (L.VarT n) = error $ "Type error: type pattern in receive clause "++
                       "contains the type variable "++show n 
typeToPat (L.ConT (L.Name _ s)) = L.AppPat 
                                (L.ConPat (L.Name undefined "Prelude.ConT"))
                                (L.LitPat $ L.StringL s)
typeToPat (L.AppT t1 t2) = L.ConPat (L.Name undefined "Prelude.AppT")
                           `L.AppPat`
                           typeToPat t1
                           `L.AppPat`
                           typeToPat t2
typeToAST :: L.TypeAST -> L.AST
typeToAST (L.VarT n) = error $ "Type error: Implicit type argument "++
                       "contains the type variable "++show n
typeToAST (L.ConT (L.Name _ s)) = --Prelude.ConT s
    L.TupleAST [L.LitAST $ L.AtomL "conT",L.LitAST $ L.StringL s]
typeToAST (L.AppT t1 t2) =
    L.TupleAST [L.LitAST $ L.AtomL "appT",typeToAST t1,typeToAST t2]
typeToAST (L.ForallT t) = typeToAST t --should this case even happen?
typeToAST t = error $ "Non-exhaustive function typeToAST: " ++ show t

patASTToPat :: L.PatAST -> A.Pattern
patASTToPat (L.VarPat (L.Name _ s)) = A.PVar s
patASTToPat app@(L.AppPat _ _) = unfoldApps app []
	where 
		unfoldApps (L.ConPat n) ps = 
                    --NOTE: removed reverse before patASTToPat here
			A.PCon (show n) (map patASTToPat ps)
		unfoldApps (L.AppPat app p) ps =
			unfoldApps app (p:ps)
patASTToPat (L.LitPat lit) = A.PLit $ lLitToALit lit
patASTToPat L.WildPat = A.PWild
patASTToPat (L.TuplePat ps) = A.PTuple $ map patASTToPat ps
patASTToPat (L.ConPat n) = A.PCon (show n) []

lLitToALit (L.StringL s) = A.LS s
lLitToALit (L.CharL c) = A.LC c
lLitToALit (L.IntegerL i) = A.LI i
lLitToALit (L.DoubleL d) = A.LD d
lLitToALit (L.AtomL s) = A.LA s
