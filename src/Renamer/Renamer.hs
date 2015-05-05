module Renamer.Renamer (transform) where

import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad 
import Data.Monoid

import Parser.AbsHopper as HPR
import AST.AST as AST
import Utils.ErrM
import Utils.BIF

transform :: HPR.Module -> Err (AST.Module (Maybe AST.Type))
transform (MMod modulename exports imports defs) = do
  let name = fromIdCon modulename
  let (adts,defs') = findADTs name defs
  defs'' <- transformDefs name defs'
  mapM_ checkLonelySignatures defs''
  exports'  <- transformExports name exports
  exports'' <- case exports' of
                 [] -> let e1 = map (\(Fun n _ _) -> n) defs''
                           e2 = M.keys adts
                       in return (e1++e2)
                 _  -> Ok exports'
  imports' <- transformImports imports
  -- TODO: Should be moved to after dependency resolver
  --checkExports expo' defs''
  return $ Mod name exports'' imports' defs'' adts

--
-- All transform* functions is a transform from the parse tree to AST
--

transformExports :: Modulename -> Exports -> Err [Identifier]
transformExports _    NEmpty      = Ok [] 
transformExports name (NExps ids) = Ok $ map go ids
  where go (NExp i) = prefix name i

transformImports :: [Import] -> Err [Identifier]
transformImports ids = Ok $ map go ids
  where go (IImport i) = fromIdCon i

transformDefs :: Modulename -> [Def] -> Err [Function (Maybe AST.Type)]
transformDefs name defs = do
  funs <- foldM go M.empty defs 
  return $ M.elems funs
  where
    go :: M.Map Identifier (Function (Maybe AST.Type)) -> Def
       -> Err (M.Map Identifier (Function (Maybe AST.Type)))
   
    go m (DSig (SSig i' t)) = let i = fromIdVar i'
                              in case M.lookup i m of
        -- There is no function
        Nothing -> 
          return $ M.insert i (Fun (prefix name i') 
                                   (Just $ transformTypes name t) 
                                   eundefined) m
        
        -- There is a function, but no signature
        Just (Fun _ Nothing e) -> 
          return $ M.insert i (Fun (prefix name i') 
                                   (Just $ transformTypes name t) 
                                   e) m
        
        -- There is a function and a signature
        _ -> fail $ "Multiple signatures for '" ++ i ++ "'"
    
    go m (DFun (FFun i' as e)) = do
      let TIdVar i = i'

      -- Add lambda if arguments
      e'' <- if null as
              then transformExpr name S.empty e
              else do pat <- mapM (transformArg name) as
                      let ctx = foldr patternToContext S.empty pat
                      e'  <- transformExpr name ctx e
                      return $ AST.ELambda pat e' 

      case M.lookup i m of
        Nothing           -> do
          -- If does pattern matching in args, generate case expression
          e''' <- case e'' of
            (AST.ELambda pat ex) | doesMatching pat -> do 
              let args = makeArgs pat
              ts <- expressionFromArgs args
              let cs = AST.ECase ts [(AST.PTuple pat, ex)]
              return $ AST.ELambda args cs
            _ -> return e''
            
          return $ M.insert i (Fun (prefix name i') Nothing e''') m
        Just (Fun _ t es) -> do
          cs <- mergeCase es e''
          return $ M.insert i (Fun (prefix name i') t cs) m

    go _ (DAdt (AAdt (TIdCon s) _ _)) = fail $ 
      "Bug! Found data declaration '" ++ s ++ "' in transformDefs"

transformTypes :: Modulename -> [HPR.Type] -> AST.Type
transformTypes name ts' = let (t:ts) = reverse ts'
                          in foldl go (go' t) ts
  where
    go :: AST.Type -> HPR.Type -> AST.Type
    go a b = AST.TCon "Prim.->" `AST.TApp` go' b `AST.TApp` a

    go' :: HPR.Type -> AST.Type
    go' (HPR.TName c ids)     = foldr go'' (AST.TCon . prim . fromIdCon $ c) ids
    go' (HPR.TVar (TIdVar v)) = AST.TVar v
    go' (HPR.TTuple ((TTTuple t):ts)) = foldr go''' (transformTypes name t) ts

    go'' :: HPR.TypeArg -> AST.Type -> AST.Type
    go'' (TTAId (ICon (IdConNQ (TIdCon c)))) b = b `AST.TApp` (AST.TCon $ prim c)
    go'' (TTAId (ICon (IdConQ (TQIdCon c)))) b = b `AST.TApp` (AST.TCon c)
    go'' (TTAId (IVar v))            b = b `AST.TApp` (AST.TVar . fromIdVar $ v)
    go'' (TTATuple ((TTTuple t):ts)) b = b `AST.TApp` foldr go''' (transformTypes name t) ts

    go''' :: HPR.TypeTuple -> AST.Type -> AST.Type
    go''' (TTTuple ts) b = b `AST.TApp` (transformTypes name ts)

    -- Prefix primitive types with Prim module instead of current module
    prim "Int"    = "Prim.Int"
    prim "Double" = "Prim.Double"
    prim "Char"   = "Prim.Char"
    prim "String" = "Prim.String"
    prim "Atom" = "Prim.Atom"
    prim "Number" = "Prim.Number"
    prim s        = name ++ "." ++ s

transformExpr :: Modulename -> S.Set Identifier -> HPR.Expr -> Err AST.Expression
transformExpr name ctx e = case e of
  HPR.EId i -> Ok $ case i of
    HPR.ICon i' -> AST.ECon $ prefix name i'
    HPR.IVar i' -> AST.EVar $ if S.member (fromIdVar i') ctx 
                                then fromIdVar i' 
                                else prefix name i'
  
  HPR.EPrim p -> Ok $ AST.ELit $ case p of
    HPR.IInteger i -> LI i
    HPR.IDouble d  -> LD d
    HPR.IChar c    -> LC c
    HPR.IString s  -> LS s

  -- To look after BIFs, this is converted and run again
  HPR.EInfix a (IdOpr op) b -> transformExpr name ctx $ HPR.EApp
                                            (HPR.EApp
                                              (HPR.EId 
                                                (HPR.IVar 
                                                  (IdVarNQ
                                                    (TIdVar op))))
                                              a)
                                            b

  HPR.EApp a b       -> do a' <- transformExpr name ctx a
                           b' <- transformExpr name ctx b
                           case a' of
                            AST.EVar i -> case lookupBIF (unqualify name i) of
                              Just (m,f,_) -> Ok $ AST.EApp (AST.EApp (AST.EApp (AST.EVar "Prim.apply") (AST.ELit (AST.LS m))) (AST.ELit (AST.LS f))) b'
                              _            -> Ok $ AST.EApp a' b'
                            _          -> Ok $ AST.EApp a' b'

  HPR.EOpr i         -> Ok $ AST.EVar $ prefix name i
  
  HPR.ECase a c      -> do e' <- transformExpr name ctx a
                           c' <- mapM (transformClause name ctx) c
                           Ok $ AST.ECase e' c'
  
  HPR.EIf a b c      -> do a' <- transformExpr name ctx a
                           b' <- transformExpr name ctx b
                           c' <- transformExpr name ctx c
                           Ok $ AST.ECase a' [(AST.PCon "True"  [], b')
                                             ,(AST.PCon "False" [], c')]

  HPR.ELambda ps a   -> do ps' <- mapM (transformPat name) ps
                           let ctx' = foldr patternToContext ctx ps'
                           a'  <- transformExpr name ctx' a
                           Ok $ AST.ELambda ps' a' 
                           
  where app (Bad m) _      = Bad m
        app _      (Bad m) = Bad m 
        app (Ok a) (Ok b)  = Ok $ AST.EApp a b

transformPat :: Modulename -> HPR.Pat -> Err AST.Pattern
transformPat name p = case p of
  HPR.PCon i  -> Ok $ AST.PCon (prefix name i) []
  HPR.PVar (TIdVar i) -> Ok $ AST.PVar i

  HPR.PPrim p -> Ok $ AST.PLit $ case p of
    HPR.IInteger i -> LI i
    HPR.IDouble d  -> LD d
    HPR.IString s  -> LS s
    HPR.IChar c    -> LC c

  HPR.PWild   -> Ok $ AST.PWild

  HPR.PTuple [p] -> transformPatTuple name p
  HPR.PTuple ps  -> do ps' <- mapM (transformPatTuple name) ps
                       Ok $ AST.PTuple ps'

transformPatTuple :: Modulename -> HPR.PatTuple -> Err AST.Pattern
transformPatTuple name a = case a of
  HPR.PTCon s qs -> do qs' <- mapM (transformPat name) qs
                       Ok $ AST.PCon (prefix name s) qs'
  HPR.PTPat p    -> transformPat name p

transformArg :: Modulename -> HPR.Arg -> Err AST.Pattern
transformArg name (APat p) = transformPat name p

transformClause :: Modulename -> S.Set Identifier -> HPR.Clause -> Err (AST.Pattern, AST.Expression)
transformClause name ctx (CClause pat e) = case pat of
  CCPPat p -> do p' <- transformPat name p
                 let ctx' = patternToContext p' ctx
                 e' <- transformExpr name ctx' e
                 Ok (p',e')

  CCPCon i p -> do p' <- mapM (transformPat name) p
                   let p'' = AST.PCon (prefix name i) p'
                   let ctx' = patternToContext p'' ctx
                   e' <- transformExpr name ctx' e
                   Ok (p'',e')


--
-- Helper functions
--

-- | Removes the modulename from a identifier
unqualify :: Modulename -> Identifier -> Identifier
unqualify name ident 
  | elem '.' ident = case go name ident of
                       Ok i  -> i
                       Bad _ -> ident
  | otherwise      = ident
  where
    go :: Modulename -> Identifier -> Err Identifier
    go []     ('.':i)            = Ok i
    go (m:ms) (i:is) | m == i    = go ms is
    go _      _                  = Bad ""

-- | Inserts a pattern variable to the variable context
patternToContext :: Pattern -> S.Set Identifier -> S.Set Identifier
patternToContext (AST.PVar i)    s = S.insert i s
patternToContext (AST.PCon _ ps) s = foldr patternToContext s ps
patternToContext (AST.PTuple ps) s = foldr patternToContext s ps
patternToContext _               s = s

-- | Find ADT declarations and take them out to an own map
findADTs :: Modulename -> [HPR.Def] -> (M.Map Identifier AST.Type, [HPR.Def])
findADTs name defs = foldr go (M.empty,[]) defs -- reverse? 
  where 
    go :: HPR.Def 
       -> (M.Map Identifier AST.Type,[HPR.Def]) 
       -> (M.Map Identifier AST.Type,[HPR.Def])

    -- Put ADT in map
    go d (m,ds) = case d of 
      DAdt (AAdt (TIdCon t) vars cons) -> 
        let m' = M.fromList $ map (dataToSignature name t vars) cons
        in (m <> m', ds)
      
      -- Otherwise skip
      d' -> (m,d':ds)


-- | Convert a data constructor to signature
--   The second argument is the last part of the signature
dataToSignature :: Modulename -> Constructor -> [AdtVar] 
                -> AdtCon -> (Constructor, AST.Type)
dataToSignature name ty tyvars adtcon = (prefix name con, types)
  where
    (ACCon con conargs) = adtcon
    types = transformTypes name $ map go' conargs 
                               ++ [TName (IdConNQ (TIdCon ty)) (map go tyvars)]

    go :: AdtVar -> TypeArg
    go (AVVar i) = TTAId (IVar (IdVarNQ i))

    go' :: AdtArg -> HPR.Type
    go' (AAId i)        = HPR.TName i []
    go' (AAVar i)       = HPR.TVar i
    go' (AATuple tup)   = HPR.TTuple $ map go'' tup

    go'' :: AdtArgTuple -> TypeTuple
    go'' (AATCon i a as) = TTTuple [HPR.TName i (map go''' (a:as))]
    go'' (AATArg a)      = TTTuple [go' a]

    go''' :: AdtArg -> TypeArg
    go''' (AAId i)      = TTAId (ICon i)
    go''' (AAVar i)     = TTAId (IVar (IdVarNQ i))
    go''' (AATuple aat) = TTATuple $ map go'' aat

-- | A temporary expression representing a function without an expression yet
eundefined :: Expression
eundefined = AST.EVar "undefined" 

-- | Check that there are no signatures without function definitions
checkLonelySignatures :: Function (Maybe AST.Type) -> Err ()
checkLonelySignatures (Fun i (Just s) e) = case e == eundefined of
  True -> Bad $ "Lonley signature '" ++ i ++ " :: " ++ show s ++ "'" 
  _    -> Ok ()
checkLonelySignatures _ = Ok ()

-- | Convert two pattern matching functions to a case expression
mergeCase :: Expression -> Expression -> Err Expression
mergeCase a b = case (a,b) of

  -- Replace eundefined with expression
  (c,d) | c == eundefined -> Ok d

  -- Add new clause to case, bugfix checks if first argument is generated 
  (AST.ELambda ps@(AST.PVar ('_':_):_) (AST.ECase e cs), (AST.ELambda ps' e')) ->
    if length ps == length ps'
      then Ok $ AST.ELambda ps (AST.ECase e (cs++[(AST.PTuple ps', e')]))
      else Bad $ "Mismatched number of arguments in patternmatching when adding"
              ++ show ps' ++ " -> " ++ show e' ++ " to case clause"

  -- Convert two lambdas to case expression
  (AST.ELambda ps e, AST.ELambda ps' e') ->
    if length ps == length ps'
      then do let as = makeArgs ps
              ts <- expressionFromArgs as
              let cs = AST.ECase ts [(AST.PTuple ps, e), (AST.PTuple ps', e')]
              Ok $ AST.ELambda as cs
      else Bad $ "Mismatched number of arguments in patternmatching between '\\"
              ++ show ps ++ " -> " ++ show e ++ "' and '\\"
              ++ show ps' ++ " -> " ++ show e' ++ "'" 

  -- Give over-shadowing error
  (c,d) -> Bad $ "Expression '" ++ show c ++ "' over-shadows '"
              ++ show d ++ "' in pattern matching"

-- | Generate arguments for lambdas 
makeArgs :: [Pattern] -> [Pattern]
makeArgs = zipWith (\n _ -> AST.PVar $ "_arg" ++ show n) ([1..] :: [Integer])

-- | Convert PTuple to ETuple
-- TODO: Make this code safe
expressionFromArgs :: [Pattern] -> Err Expression
expressionFromArgs ps = do
  ps' <- mapM go ps
  Ok $ ETuple ps'
  where go (AST.PVar n) = Ok $ AST.EVar n
        go e = Bad $ "That shouldn't be in expressionFromArgs: " ++ show e


doesMatching :: [Pattern] -> Bool
doesMatching = any matching
  where matching (AST.PVar _) = False
        matching _        = True


class Prefix a where
  prefix :: Modulename -> a -> Identifier


instance Prefix TIdVar where
  prefix n (TIdVar i) = n ++ "." ++ i
  
instance Prefix TQIdVar where
  prefix _ (TQIdVar i) = i

instance Prefix TIdCon where
  prefix _ (TIdCon "True") = "True"
  prefix _ (TIdCon "False") = "False"
  prefix n (TIdCon i)       = n ++ "." ++ i

instance Prefix TQIdCon where
  prefix _ (TQIdCon i) = i

instance Prefix IdVar where
  prefix n (IdVarNQ i) = prefix n i
  prefix n (IdVarQ i)  = prefix n i

instance Prefix IdCon where
  prefix n (IdConNQ i) = prefix n i
  prefix n (IdConQ i)  = prefix n i

instance Prefix IdOpr where
  prefix n (IdOpr i) = n ++ "." ++ i

instance Prefix Id where
  prefix n (ICon i) = prefix n i
  prefix n (IVar i) = prefix n i

-- | Take out the identifier from an Id
fromId :: Id -> Identifier
fromId (ICon i) = fromIdCon i
fromId (IVar i) = fromIdVar i

fromIdCon :: IdCon -> Identifier
fromIdCon (IdConNQ (TIdCon i)) = i
fromIdCon (IdConQ (TQIdCon i)) = i

fromIdVar :: IdVar -> Identifier
fromIdVar (IdVarNQ (TIdVar i)) = i
fromIdVar (IdVarQ (TQIdVar i)) = i

