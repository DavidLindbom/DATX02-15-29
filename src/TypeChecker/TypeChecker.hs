{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses,
FlexibleInstances #-}
module TypeChecker.TypeChecker where

import Control.Monad.State
import AST.Liam_TC_AST 
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Foldable as F
import qualified Control.Monad.Reader as R
import qualified Data.Graph as G
import Control.Arrow ((***))
import Data.Maybe (fromJust)
import Data.List (partition,union)

--NOTE: I assume String == Prim.String,
--Atom == Prim.Atom, (->) == Prim.(->) etc.
--Change depending on hopper module structure.
--Tuple-cons: Prim.*
--Tuplr-nil: Prim.()??

import TypeChecker.Convert (moduleToRenamed,tcModToModule)
import Utils.ErrM (Err(..))
import qualified AST.AST as A
typeCheck :: A.Module (Maybe A.Signature) -> Err (A.Module A.Signature)
typeCheck mod = case typecheckModule $ moduleToRenamed mod of
                  Left s -> Bad s
                  Right tcmod -> Ok $ tcModToModule tcmod
                  
typecheckModule :: RenamedModule -> Either String TCModule
typecheckModule rnm = do 
  --NOTE: I HARDCODED THE TYPE OF apply :: String -> String -> a -> b
  --here.
  names'types <- typecheck ((Name Nothing "apply",
                             ForallT $ foldr1 arrowtype
                                         [prim"String",prim"String",
                                          tyVar "a",tyVar "b"])
                            :cons rnm) (defs rnm) --todo TC transforms code
  return $ TCModule 
             (Name (Just $ init $ modId rnm)$last$modId rnm)
             (exports rnm)
             (map constructorDef (cons rnm) ++
              recombineTypes'Sigh names'types (defs rnm))
      where
        constructorDef (n,t) = (n,argsToValue n $ arity t,t)
        --TODO make n fully qualified!
        argsToValue n 0 = nameToAtom n
        argsToValue n ar = LamAST 
                           (map VarPat vs)
                           $ TupleAST $
                           nameToAtom n:
                           map Named vs
                               where
                                 vs = [name $ "x"++show n |
                                       n <- [1..ar]]
        nameToAtom = LitAST . AtomL . show
        arity (AppT (AppT (ConT(Name(Just["Prim"])"->")) _) t) = 
            1 + arity t
        arity _ = 0
        recombineTypes'Sigh ns'ts  = 
            map (\(n,ast,_) -> (n,ast,fromJust $ lookup n ns'ts))
--TODO typecheck also returns modified expression
typecheck :: [(Name,TypeAST)] -> --imports, constructors
             [(Name,AST,Maybe TypeAST)] -> --defs
            Either String [(Name,TypeAST)]
typecheck imps'cons vs = let (withDecls,woDecls) = ((map $ \(n,ast,Just t)->
                                                    (n,ast,t)) 
                                               ***
                                               (map $ \(n,ast,_)->(n,ast)))$
                                              partition 
                                              (\(n,ast,m)->m/=Nothing) vs
                             tmap = M.fromList $ 
                               map (\(n,_,t) -> (n,t)) withDecls 
                               ++ imps'cons
                             sccs :: [[(Name,AST)]]
                             sccs = map G.flattenSCC $ G.stronglyConnComp $ 
                               map (\(n,ast)->
                                        ((n,ast),n,namesOccuring ast)) 
                               woDecls
                             names = map (\(n,_,_) -> n) vs
                        in runTCMonad (typecheckDefs names sccs withDecls)
                           tmap (0,M.empty)
typecheckDefs ns sccs wd = do tmap <- typecheckSCCs sccs
                              R.local (const tmap) $ checkTypes wd
                              return (getVals ns tmap)
                                  where
                                    getVals [] _ = []
                                    getVals (n:ns) map = 
                                        (n,case M.lookup n map of
                                             Nothing -> error $
                                                        "Unexpected Nothing "++
                                                        "in TC.typecheckDefs"
                                             Just x -> x):
                                        getVals ns map
                                    checkTypes [] = return ()
                                    checkTypes ((n,ast,t):nastts) = do
                                      put (0,M.empty) --start new session
                                      t' <- tcExpr ast
                                      t'' <- getFullType' S.empty t'
                                      if t == t'' 
                                       then return ()
                                       else do
                                         p <- get
                                         lift$lift$Left 
                                            ("Incorrect type signature: "
                                             ++show (n,t,t',p))
                                                
                                                
                                          
typecheckSCCs :: [[(Name,AST)]] -> 
                  TCMonad (M.Map Name TypeAST)
typecheckSCCs [] = R.ask
typecheckSCCs (scc:sccs) = do map <- typecheckSCC scc
                              R.local (const map) (typecheckSCCs sccs)
typecheckSCC :: [(Name,AST)] -> TCMonad (M.Map Name TypeAST)
typecheckSCC nasts = do
  --given type variable to each name
  ntasts <- mapM (\(n,ast) -> do t <- newTyVar
                                 return (n,t,ast)) nasts
  let ns = map fst nasts
      nts = map (\(n,t,_)->(n,t)) ntasts
      tvAndDefs = map (\(_,t,ast)->(t,ast)) ntasts
  R.local (M.union (M.fromList nts)) $ do 
    mapM typecheckDef tvAndDefs
    --s <- get
    --m <- R.ask
    --error $ show (s,m)
    --getFullType (name "loop")
    --error "Done"
    namesTypes <- mapM getFullType ns
    ntmap <- R.ask
    put (0,M.empty)
    return $ M.fromList namesTypes `M.union` ntmap
getFullType :: Name -> TCMonad (Name,TypeAST)
getFullType n = do m <- R.ask
                   let Just tyvar = M.lookup n m  
                   t <- getFullType' S.empty tyvar
                   return $ if S.null $ tyVarNamesOf t 
                            then (n,t)
                            else (n,ForallT t)
getFullType' s (VarT n) | S.member n s = return (VarT n)
                        | otherwise = do (_,con) <- get
                                         case M.lookup n con of
                                           Nothing -> return $ VarT n
                                           Just t  -> getFullType' 
                                                      (S.insert n s) t
getFullType' s (AppT tf tx) = do t1 <- getFullType' s tf
                                 t2 <- getFullType' s tx
                                 return $ AppT t1 t2
getFullType' _ t = return t
  
--takes the type variable assigned to the expression and its definition
typecheckDef :: (TypeAST,AST) -> TCMonad TypeAST
typecheckDef (tv,ast) = do t <- tcExpr ast
                           unify tv t
                           return tv
tcExpr :: AST -> TCMonad TypeAST
tcExpr (LitAST lit) = return $ litType lit
tcExpr (Named n) = do map <- R.ask
                      case M.lookup n map of
                       Just (ForallT t) -> do t' <- newVarNames t
                                              return t'
                       Just t -> return t
                       Nothing -> complain $
                                  "Unexpected Nothing in tcExpr "++
                                  "when looking up " ++ show n
tcExpr (AppAST f x) = do a <- newTyVar
                         b <- newTyVar
                         let a_arrow_b = prim "->" `AppT` a `AppT` b
                         tf <- tcExpr f
                         tx <- tcExpr x
                         unify tf a_arrow_b
                         unify tx a
                         return b
tcExpr (LamAST [] body) = tcExpr body 
-- \->body is not valid AST, constructed within the typechecker only
tcExpr (LamAST (p:ps) body) = do 
  let pns = namesOccP p
  pnts <- mapM (\n -> do t <- newTyVar
                         return (n,t)) pns
  R.local (M.union (M.fromList pnts)) $ do a <- tcPattern p
                                           b <- tcExpr (LamAST ps body)
                                           return (AppT (prim"->") a
                                                   `AppT` b)
tcExpr (IfAST i t e) = do ti <- tcExpr i
                          unify ti (prim"Bool")
                          tt <- tcExpr t
                          te <- tcExpr e
                          unify tt te
                          return tt
tcExpr (TupleAST []) = return $ prim "()"
tcExpr (TupleAST (x:tup)) = do tx <- tcExpr x
                               ttup <- tcExpr $ TupleAST tup
                               return $ prim"*" `AppT` tx `AppT` ttup
--problem: variables in case patterns must be put in type map.
tcExpr (CaseAST exp clauses) = do
  t:ts <- mapM (\(pat,res) -> 
                     tcExpr (LamAST[pat]res `AppAST` exp))
           clauses
  t <- foldM unifyT t ts
  return t
    where
      unifyT t1 t2 = unify t1 t2 >> return t1
tcExpr WildAST = newTyVar
tcExpr (AsAST a b) = do ta <- tcExpr a
                        tb <- tcExpr b
                        unify ta tb
                        return ta
--Todo receive uses different tcPattern function
tcPattern :: PatAST -> TCMonad TypeAST
tcPattern = tcExpr . patToExpr

patToExpr :: PatAST -> AST
patToExpr p = case p of
                VarPat n -> Named n
                WildPat -> WildAST
                AppPat f x -> AppAST (patToExpr f) (patToExpr x)
                ConPat n -> Named n
                LitPat l -> LitAST l
                TuplePat xs -> TupleAST $ map patToExpr xs
                AsPat p1 p2 -> AsAST (patToExpr p1) (patToExpr p2)
                  
--make new type variable for each variable in pattern,
--locally modify name->type environment.

--adds the module prefix Prim to a type name.
prim = ConT . Name (Just ["Prim"])  

litType lit = case lit of 
                StringL _ -> prim "String"
                IntegerL _ -> prim "Integer"
                DoubleL _ -> prim "Double"
                CharL _ -> prim "Char"
                AtomL _ -> prim "Atom"                               

namesOccuring :: AST -> [Name]
namesOccuring (Named n) = [n]
namesOccuring (AppAST f x) = namesOccuring f `union` namesOccuring x
namesOccuring (LamAST [] x) = namesOccuring x
namesOccuring (LamAST (p:ps) x) = filter (not . flip elem (namesOccP p)) 
                                  (namesOccuring $ LamAST ps x)
namesOccuring _ = []

namesOccP :: PatAST -> [TyVarName]
namesOccP (VarPat n) = [n]
namesOccP (AppPat f x) = namesOccP f `union` namesOccP x
namesOccP (AsPat v p) = namesOccP v `union` namesOccP p
namesOccP (TuplePat ps) = foldr union [] $ map namesOccP ps
namesOccP _ = []

--make imports into map
--extract values with type declarations,
--put their types in the map;
--get scc's of the rest; typecheck them
--get scc's of the ones with typedecs, check if
--equal to decs

--typecheck scc:
--make a variable for each expression;
--mapM typecheckExpr (flattenSCC scc)

type Constraint = M.Map TyVarName TypeAST
type TCMonad a = R.ReaderT (M.Map Name TypeAST) 
    (StateT (Int,Constraint) (Either String)) a
runTCMonad :: TCMonad a -> 
              (M.Map Name TypeAST) -> (Int,Constraint) -> 
              Either String a
runTCMonad m r s = do (v,s') <- runStateT (R.runReaderT m r) s
                      return v
--f = (f :: t ... f :: t ... g :: a)
--if g has type signature, use that
--lit: T, {}
--f x: unify f with (a -> b), x with b

--r => variables in receive r
--have upper type. New variables
--have new type

--cyclical types not ok

--unifies instantiated types, no forall
unify :: TypeAST -> TypeAST -> TCMonad ()
unify (VarT x) (VarT y) | x == y = return ()
unify (VarT x) t = checkNoCycles x t
unify t (VarT x) = checkNoCycles x t
unify (AppT a b) (AppT c d) = unify a c >> unify b d
unify (ConT n1) (ConT n2) | n1 == n2 = return ()
unify t1 t2 = lift $ lift $ Left $ 
              concat["Cannot unify: (",show t1,")(",show t2,")"]
        
checkNoCycles :: TyVarName -> TypeAST -> TCMonad ()      
checkNoCycles tvn newType = do
  (i,map) <- get
  case M.lookup tvn map of
    Nothing -> 
        do let state = (i,M.insert tvn newType map)
           lift $ put state
           tryToFindCycle tvn tvn
           lift $ put state
           
    Just t ->
        unify t newType
  where tryToFindCycle from at =  
            do (_,con) <- get
               case M.lookup at con of
                 Nothing -> return () --visited it already alt. not bound
                 Just t -> let names = tyVarNamesOf t
                           in if t == VarT from then return () else
                                  if S.member from names
                                  then do p <- get
                                          lift $ lift $ Left $ "Found cycle: "
                                             ++ show (from,at,t,p)
                                  else do modify $ id *** M.delete at 
                                          mapM_ (tryToFindCycle from) $ 
                                            S.elems names 
  
newVarNames :: TypeAST -> TCMonad TypeAST
newVarNames t = do (i,con) <- get
                   put (i,M.empty)
                   t' <- newVarNames' t
                   modify $ id *** const con
                   return t'
newVarNames' :: TypeAST -> TCMonad TypeAST  
newVarNames' (VarT x) = do (i,vs) <- get
                           case M.lookup x vs of
                             Just t -> return t
                             _ -> let t = tyVar $ "t"++show i
                                  in do put (i+1,M.insert x t vs)
                                        return t
newVarNames' c@(ConT _) = return c
newVarNames' (AppT a b) = do a' <- newVarNames' a
                             b' <- newVarNames' b
                             return $ AppT a' b'

newTyVar :: TCMonad TypeAST
newTyVar = do (i,_) <- get
              modify $ (+1) *** id
              return $ tyVar $ "t"++show i

--assumes they share no type variable names
isInstanceOf :: TypeAST -> TypeAST -> Bool
t1 `isInstanceOf` t2 = let readf :: TCMonad () -> 
                                    StateT (Int,Constraint) (Either String) ()
                           readf = flip R.runReaderT M.empty 
                           statef = flip execStateT (0,M.empty) 
                       in case statef $ readf (unify t1 t2)of
                         Left _ -> False
                         Right (_,con) -> F.all (\tvn -> not $ M.member tvn con)
                                          (tyVarNamesOf t2) 
tyVarNamesOf (VarT v) = S.singleton v
tyVarNamesOf (AppT a b) = S.union (tyVarNamesOf a) $ tyVarNamesOf b
tyVarNamesOf _ = S.empty

name = Name Nothing 

complain = lift . lift . Left

--DEBUGGING ||
--          \/
fullTypeTest :: TCMonad TypeAST
fullTypeTest = do let t0 = tyVar "t0"
                      t1 = tyVar "t1"
                      t2 = tyVar "t2"
                  put (3,M.fromList [(Name Nothing "t0",t1),
                                     (Name Nothing "t1",prim"Atom"),
                                     (Name Nothing "t2",t0)])
                  getFullType' S.empty t1

example = --let id = LamAST [VarPat $ name "x"] (Named $ name "x") in 
                     [--(name "id",id,Nothing),
                       --(name "atom",AppAST (Named $ name "id")
                           --      (LitAST (AtomL "atom")),Just $ prim "Atom"),
                      def "loop" (LamAST [VarPat $ name "x"] 
                                   (AppAST (Named $ name "loop")
                                           (Named $ name "x")))
                     --,def "loop2" (Named $ name "loop2")
                     ] 
    where def str ast = (name str,ast,Nothing)
shrunkEx = runTCMonad (tcExpr (LamAST [VarPat $ name "x"] 
                               (AppAST (Named $ name "loop")
                                (Named $ name "x"))))
           (M.singleton (name "loop") (tyVar "t0")) (1,M.empty)
           
--cycle: 
--t2 : 
--t0 = (t2 -> t3)
--t1 = t2

--loop = t0
--x = t1

--t0 = (a -> b)

unifyLoopTest = do [t0,t1,t2,t3] <- sequence $ replicate 4 newTyVar
                   unify t0 (arrowtype t2 t3)
                   unify t1 t2
                   mapM (getFullType' S.empty) [t0,t1,t2,t3]


arrowtype a b = AppT (prim"->") a `AppT` b


 