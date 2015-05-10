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
import Data.Char (toLower)

--NOTE: I assume String == Prim.String,
--Atom == Prim.Atom, (->) == Prim.(->) etc.
--Change depending on hopper module structure.

import TypeChecker.Convert (moduleToRenamed,tcModToModule)
import Utils.ErrM (Err(..))
import qualified AST.AST as A
import Data.Char (isUpper)
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef

nothingIORef = unsafePerformIO $ newIORef (Left "doesn't matter")

typeCheck :: A.Module (Maybe A.Type) -> Err (A.Module A.Type)
typeCheck mod = case typecheckModule $ moduleToRenamed mod of
                  Left s -> Bad s
                  Right tcmod -> Ok $ tcModToModule tcmod
                  
typecheckModule :: RenamedModule -> Either String TCModule
typecheckModule rnm = do
  --NOTE: I HARDCODED THE TYPE OF apply :: String -> String -> Number -> a
  --here.
  names'types <- typecheck ((Name nothingIORef "Prim.apply",
                             ForallT $ foldr1 arrowtype
                                         [prim"String",prim"String",
                                              prim"Number",
                                          tyVar "a"])
                            :(map (id *** ForallT)$ cons rnm ++ imports rnm)) 
                 --IT NEVER HURTS TO MAKE A TYPE SIGNATURE forall! Or does it?
                 (map (\(n,a,mt) -> (n,a,fmap ForallT mt)) $ defs rnm)
  return $ TCModule 
             (Name nothingIORef $ modId rnm)
             (exports rnm)
             (map constructorDef (cons rnm) ++
              recombineTypes'Sigh names'types (defs rnm))
      where
        constructorDef (n,t) = (n,argsToValue n $ arity t,ForallT t)
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
        nameToAtom (Name _ s) = LitAST $ AtomL $ case unqualifiedName s of
                                                   ':':s' -> s'
                                                   upper:s' -> toLower upper:s'
                                                  
        arity (ForallT t) = arity t
        arity (AppT (AppT (ConT(Name _ "Prim.->")) _) t) = 
            1 + arity t
        arity _ = 0
        recombineTypes'Sigh ns'ts  = 
            map (\(n,ast,_) -> (n,ast,fromJust $ lookup n ns'ts))
--TODO typecheck also returns modified expression: DONE!
--Need to treat implicit specially now
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
                             addForalls = map (\(n,a,t) -> (n,a,
                                                       case t of
                                                         ForallT _ -> t
                                                         _ -> ForallT t))
                        in runTCMonad (typecheckDefs names sccs 
                                       (addForalls withDecls))
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
                                    checkTypes ((n,ast,sigt):nastts) = do
                                      let sigt2 = case sigt of
                                                    ForallT (Implicit t1 t2)->
                                                        ForallT (arrowtype
                                                                 (ConT
                                                                  (Name 
                                                                   undefined $
                                                                   "Prelude."++
                                                                   "Type")) t2)
                                                    ForallT _ -> sigt
--NOTE: I don't get why tcExpr terminates without a signature,
--but loops with. I'll just call typeCheckSCCs here and hope it does the trick.
                                      --texpr <- tcExpr ast
                                      texpr <- fmap (fromJust . M.lookup n) $
                                               typecheckSCC [(n,ast)]
                                      nsigt <- newVarNames sigt2
                                      unify texpr nsigt
                                      nsigt `checkIsInstanceOf` texpr
                                      checkTypes nastts
--checks that the type signature is less general than
--the inferred type, e.g if you accidentally give
--map the type (a->a) -> List a -> List a
--and the signature (a->b) -> List a -> List b,
--then checkIsLessGeneral will complain.
--It does this by checking that each type variable
--in the type signature refers to a different type
--variable in the expression's type when
--you unify them.
checkIsInstanceOf :: TypeAST -> TypeAST -> TCMonad ()
checkIsInstanceOf sig t = do nsig <- newVarNames sig
                             fullT <- fullType t
                             if nsig `isInstanceOf` fullT
                                then return ()
                                else complain $ 
                                         "Type signature: \n"++
                                         show nsig ++ "\n" ++
                                         "is not an instance of:  \n" ++
                                         show fullT ++ "!"
typecheckSCCs :: [[(Name,AST)]] -> 
                  TCMonad (M.Map Name TypeAST)
typecheckSCCs [] = R.ask
typecheckSCCs (scc:sccs) = do map <- typecheckSCC scc
--NOTE: SHOULDN'T IT BE UNION MAP?
                              R.local (const map) (typecheckSCCs sccs)
typecheckSCC :: [(Name,AST)] -> TCMonad (M.Map Name TypeAST)
typecheckSCC nasts = do
  --given type variable to each name
  ntasts <- mapM (\(n,ast) -> do t <- newTyVar
                                 return (n,t,ast)) nasts
  let ns = map fst nasts
      asts = map snd nasts
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
    mapM putFullTypeInIORef asts
    ntmap <- R.ask
    --NOTE: What purpose does resetting variables serve?
    --They don't interfere with each other right?
    --put (0,M.empty)
    return $ M.fromList namesTypes `M.union` ntmap
           
putFullTypeInIORef = putFT
putFT (Named (Name ioref n)) = 
    case unsafePerformIO $ readIORef ioref of
      Right implicitTypeArg ->
          do ft <- fullType implicitTypeArg
             unsafePerformIO $ writeIORef ioref (Right ft) >>
                             return (return ())
             --error $ show (n,ft,implicitTypeArg)
      Left _ -> return ()
putFT (LamAST _ ast) = putFT ast
putFT (AppAST f x) = putFT f >> putFT x
putFT (CaseAST ast cases) = 
    mapM (putFT . snd) cases >>
         putFT ast
putFT (TupleAST asts) = mapM putFT asts >> return ()
putFT (UnsafeReceive cases (_,ast)) = mapM (putFT . snd) cases >>
                                      putFT ast

putFT (Receive iorefs'cases (_,ast)) =
    mapM (\(ioref,_,ast) -> do
              ft <- fullType $ unsafePerformIO $ readIORef ioref
              unsafePerformIO $ writeIORef ioref ft >> return (return ())
              putFT ast)
    iorefs'cases >>
    putFT ast
putFT _ = return ()

--NOTE: getFullType may have had bug where
--e.g. AppT t1 t2, t1 ==> t2, t2 ==> t1
--would be converted into AppT t2 t1 and not
--AppT t2 t2 as it should have been.
--getFullType' now returns its set of 
--mentioned variables in order to prevent that.
--NOTE: Changed back, it induced a bug I think.
getFullType :: Name -> TCMonad (Name,TypeAST)
getFullType n = do m <- R.ask
                   let Just tyvar = M.lookup n m  
                   (_,t) <- getFullType' S.empty tyvar
                   return $ if S.null $ tyVarNamesOf t 
                            then (n,t)
                            else (n,ForallT t)
getFullType' s (VarT n) | S.member n s = return (s,VarT n)
                        | otherwise = do (_,con) <- get
                                         case M.lookup n con of
                                           Nothing -> return (s,VarT n)
                                           Just t  -> getFullType' 
                                                      (S.insert n s) t
getFullType' s (AppT tf tx) = do (_,t1) <- getFullType' s tf
                                 (_,t2) <- getFullType' s tx
                                 return (s,AppT t1 t2)
getFullType' s (ForallT t) = getFullType' s t
getFullType' s (Implicit t1 t2) = getFullType' s t2
getFullType' s t = return (s,t)

--Dirty hack to get full type (i.e. the type with all variables
--expanded as far as possible) given just a TypeAST
fullType :: TypeAST -> TCMonad TypeAST
fullType t = R.local (M.insert (name "Grammatically incorrect name!") t) $
             do (_,t) <- getFullType (name "Grammatically incorrect name!")
                return t
  
--takes the type variable assigned to the expression and its definition
typecheckDef :: (TypeAST,AST) -> TCMonad TypeAST
typecheckDef (tv,ast) = do t <- tcExpr ast
                           unify tv t
                           --putFT ast --wrong place to put this?"
                           return tv
tcExpr :: AST -> TCMonad TypeAST
tcExpr (LitAST lit) = return $ litType lit
tcExpr (Named n@(Name ioref _)) = do 
  map <- R.ask
  case M.lookup n map of
    Just (ForallT t) -> do t' <- newVarNames t
                           case t' of
                             Implicit ta tb -> do
                                 unsafePerformIO (writeIORef ioref (Right ta)
                                                 >> return (return ()))
                                 --error $ show (t,t')
                             _ -> return ()
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
tcExpr (CaseAST exp clauses) = do
  t:ts <- mapM (\(pat,res) -> 
                     tcExpr (LamAST[pat]res `AppAST` exp))
           clauses
  t <- foldM unifyT t ts
  return t
     
tcExpr WildAST = newTyVar
tcExpr (AsAST a b) = do ta <- tcExpr a
                        tb <- tcExpr b
                        unify ta tb
                        return ta
tcExpr (UnsafeReceive cases (timeout,exp)) = do
  a <- tcExpr exp
  t:ts <- mapM (\(pat,res) ->
               do (ConT (Name _ "Prim.->") `AppT` _ `AppT` t) <- 
                      tcExpr (LamAST[pat] res)
                  return t) cases
  tret <- foldM unifyT a (t:ts)
  return $ ioType tret
tcExpr (Receive iorefs'cases (timeout,exp)) = do
  a <- tcExpr exp
  t:ts <- mapM (\(ioref,pat,res) ->
                    do (ConT (Name _ "Prim.->") `AppT` patT `AppT` retT) <-
                           tcExpr (LamAST[pat]res)
                       unsafePerformIO (writeIORef ioref patT >> 
                                        return (return ()))
                       return retT) iorefs'cases
  tret <- foldM unifyT a (t:ts)
  return $ ioType tret
--Todo receive uses different tcPattern function
tcPattern :: PatAST -> TCMonad TypeAST
tcPattern = tcExpr . patToExpr

unifyT t1 t2 = unify t1 t2 >> return t1
ioType t = (AppT (ConT (Name (error "Don't read this IORef!") "Prelude.IO")) t)

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
prim s = ConT $ Name (error "Don't inspect TypeChecker.prim's IORef field!") 
         $ "Prim."++s  

litType lit = case lit of 
                StringL _ -> prim "String"
                IntegerL _ -> prim "Number"
                DoubleL _ -> prim "Number"
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

--unifies instantiated types, now handles forall
unify :: TypeAST -> TypeAST -> TCMonad ()
unify (ForallT t) t2 = do t' <- newVarNames t
                          unify t t2
unify t (ForallT t2) = do t2' <- newVarNames t2
                          unify t t2'
unify (Implicit t1 t2) t3 = unify t2 t3
unify t1 (Implicit t2 t3) = unify t1 t3
unify (VarT x) (VarT y) | x == y = return ()
unify (VarT x) t = checkNoCycles x t
unify t (VarT x) = checkNoCycles x t
unify (AppT a b) (AppT c d) = unify a c >> unify b d
unify (ConT n1) (ConT n2) | n1 == n2 = return ()
unify t1 t2 = lift $ lift $ Left $ 
              concat["Cannot unify: (",show t1,")\n(",show t2,")"]
        
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
newVarNames' (ForallT t) = newVarNames' t
newVarNames' (Implicit t1 t2) = do t1' <- newVarNames' t1
                                   t2' <- newVarNames' t2
                                   return $ Implicit t1' t2'
newVarNames' (VarT x) = do (i,vs) <- get
                           case M.lookup x vs of
                             Just t -> return t
                             _ -> {-let t = tyVar $ "t"++show i
                                  in do put (i+1,M.insert x t vs)
                                        return t-}
                                              do t' <- newTyVar
                                                 modify (id***M.insert x t')
                                                 return t'
newVarNames' c@(ConT _) = return c
newVarNames' (AppT a b) = do a' <- newVarNames' a
                             b' <- newVarNames' b
                             return $ AppT a' b'

newTyVar :: TCMonad TypeAST
newTyVar = do (i,_) <- get
              modify $ (+1) *** id
              return $ tyVar $ "t"++show i

--assumes they share no type variable names
--t1 is an instance of t2 if each of its 
--type variables points to a unique t.v.
--if t1 is unified with t2:
--e.g. (b -> c) > (a -> a) bc. b == a && c == a
isInstanceOf :: TypeAST -> TypeAST -> Bool
t1 `isInstanceOf` (ForallT t2) = t1 `isInstanceOf` t2
t1 `isInstanceOf` t2 = 
    let 
        readf :: TCMonad () -> 
                 StateT (Int,Constraint) (Either String) ()
        readf = flip R.runReaderT M.empty 
        statef = flip execStateT (0,M.empty) 
    in
      case statef $ readf (unify t2 t1)of
        Left _ -> False
        Right (_,con) -> F.all (\tvn -> not $ M.member tvn con)
                         (tyVarNamesOf t1)



tyVarNamesOf (VarT v) = S.singleton v
tyVarNamesOf (AppT a b) = S.union (tyVarNamesOf a) $ tyVarNamesOf b
tyVarNamesOf _ = S.empty

name = Name nothingIORef 

complain = lift . lift . Left

--DEBUGGING ||
--          \/
--Commented it out instead of updating it to work
--with new IORef field in Name
{-
fullTypeTest :: TCMonad (S.Set TyVarName,TypeAST)
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

-}
arrowtype a b = AppT (prim"->") a `AppT` b

--Code replication :(
 
--Breaks a name into its module prefix and its unqualified name.
--Because all names now have a module prefix, it no longer returns a Maybe.
strToName :: String -> (String, String)
strToName = (init' *** id) . strToName'
            where init' [] = ""
                  init' s = init s
strToName' (c:s) | isUpper c = case break (=='.') (c:s) of
                                (con,"") -> ("", con)
                                (mod,'.':s') -> addModule mod $ strToName' s'
                | otherwise = ([], c:s )
                where
                  addModule m (mId, s) = (m++"."++mId, s)
unqualifiedName = snd . strToName