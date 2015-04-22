module Renamer.Renamer (transform) where

import qualified Data.Map as M
import Control.Monad 
import Data.Monoid

import Parser.AbsHopper as HPR
import AST.AST as AST
import Utils.ErrM
import Utils.BIF

transform :: HPR.Module -> Err (AST.Module (Maybe AST.Type))
transform (MMod (IdCon name) exports imports defs) = do
  let (adts,defs') = findADTs name defs
  defs'' <- transformDefs name defs'
  mapM_ checkLonelySignatures defs''
  exports'  <- transformExports name exports
  exports'' <- case exports' of
                 [] -> let e1 = map (\(Fun _ n _ _) -> n) defs''
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
  where go (NExp i) = name ++ "." ++ fromId i

transformImports :: [Import] -> Err [Identifier]
transformImports ids = Ok $ map go ids
  where go (IImport (IdCon i)) = i

transformDefs :: Modulename -> [Def] -> Err [Function (Maybe AST.Type)]
transformDefs name defs = do
  funs <- foldM go M.empty defs 
  return $ M.elems funs
  where
    go :: M.Map Identifier (Function (Maybe AST.Type)) -> Def
       -> Err (M.Map Identifier (Function (Maybe AST.Type)))
   
    go m (DSig (SSig (IdVar i) t)) = case M.lookup i m of
        -- There is no function
        Nothing -> 
          return $ M.insert i (Fun name i (Just $ transformTypes name t) eundefined) m
        
        -- There is a function, but no signature
        Just (Fun _ _ Nothing e) -> 
          return $ M.insert i (Fun name i (Just $ transformTypes name t) e) m
        
        -- There is a function and a signature
        _ -> fail $ "Multiple signatures for '" ++ i ++ "'"
    
    go m (DFun (FFun (IdVar i) as e)) = do
      e' <- transformExpr e

      -- Add lambda if arguments
      e'' <- if null as
              then return e'
              else do pat <- mapM transformArg as
                      return $ AST.ELambda pat e'     

      case M.lookup i m of
        Nothing           -> do
          -- If does pattern matching in args, generate case expression
          e''' <- case e'' of
            (AST.ELambda pat ex) | doesMatching pat -> do 
              let args = makeArgs pat
              ts <- expressionFromArgs args
              let cs = AST.ECase ts [(AST.PTuple pat, ex)]
              return $ AST.ELambda args cs
            _ -> return e''
            
          return $ M.insert i (Fun name i Nothing e''') m
        Just (Fun _ _ t es) -> do
          cs <- mergeCase es e''
          return $ M.insert i (Fun name i t cs) m

    go _ (DAdt (AAdt (IdCon s) _ _)) = fail $ 
      "Bug! Found data declaration '" ++ s ++ "' in transformDefs"

transformTypes :: Modulename -> [HPR.Type] -> AST.Type
transformTypes name ts' = let (t:ts) = reverse ts'
                          in foldl go (go' t) ts
  where
    go :: AST.Type -> HPR.Type -> AST.Type
    go a b = AST.TCon "Prim.->" `AST.TApp` go' b `AST.TApp` a

    go' :: HPR.Type -> AST.Type
    go' (HPR.TName (IdCon c) ids)     = foldr go'' (AST.TCon $ prim c) ids
    go' (HPR.TVar (IdVar v))          = AST.TVar v
    go' (HPR.TTuple ((TTTuple t):ts)) = foldr go''' (transformTypes name t) ts

    go'' :: HPR.Id -> AST.Type -> AST.Type
    go'' (ICon (IdCon c)) b = b `AST.TApp` (AST.TCon $ prim c)
    go'' (IVar (IdVar v)) b = b `AST.TApp` AST.TVar v

    go''' :: HPR.TypeTuple -> AST.Type -> AST.Type
    go''' (TTTuple ts) b = b `AST.TApp` (transformTypes name ts)

    -- Prefix primitive types with Prim module instead of current module
    prim "Int"    = "Prim.Int"
    prim "Double" = "Prim.Double"
    prim "Char"   = "Prim.Char"
    prim "String" = "Prim.String"
    prim s        = name ++ "." ++ s

transformExpr :: HPR.Expr -> Err AST.Expression
transformExpr e = case e of
  HPR.EId i -> Ok $ case i of
    HPR.ICon (IdCon i') -> AST.EVar i'
    HPR.IVar (IdVar i') -> AST.ECon i'
  
  HPR.EPrim p -> Ok $ AST.ELit $ case p of
    HPR.IInteger i -> LI i
    HPR.IDouble d  -> LD d
    HPR.IChar c    -> LC c
    HPR.IString s  -> LS s

  -- To look after BIFs, this is converted and run again
  HPR.EInfix a (IdOpr op) b -> transformExpr $ HPR.EApp
                                            (HPR.EApp
                                              (HPR.EId 
                                                (HPR.IVar 
                                                  (IdVar op)))
                                              a)
                                            b

  HPR.EApp a b       -> do a' <- transformExpr a
                           b' <- transformExpr b
                           case a' of
                            AST.EVar i -> case lookupBIF i of
                              Just (m,f,_) -> Ok $ ECall m f b'
                              _            -> Ok $ AST.EApp a' b'
                            _          -> Ok $ AST.EApp a' b'

  HPR.EOpr (IdOpr i) -> Ok $ AST.EVar i
  
  HPR.ECase a c      -> do e' <- transformExpr a
                           c' <- mapM transformClause c
                           Ok $ AST.ECase e' c'
  
  HPR.EIf a b c      -> do a' <- transformExpr a
                           b' <- transformExpr b
                           c' <- transformExpr c
                           Ok $ AST.ECase a' [(AST.PCon "True"  [], b')
                                             ,(AST.PCon "False" [], c')]

  HPR.ELambda ps a   -> do a'  <- transformExpr a 
                           ps' <- mapM transformPat ps 
                           Ok $ AST.ELambda ps' a' 
                           
  where app (Bad m) _      = Bad m
        app _      (Bad m) = Bad m 
        app (Ok a) (Ok b)  = Ok $ AST.EApp a b

transformPat :: HPR.Pat -> Err AST.Pattern
transformPat p = case p of
  HPR.PId i -> Ok $ case i of
    HPR.ICon (IdCon i') -> AST.PCon i' []
    HPR.IVar (IdVar i') -> AST.PVar i'

  HPR.PPrim p -> Ok $ AST.PLit $ case p of
    HPR.IInteger i -> LI i
    HPR.IDouble d  -> LD d
    HPR.IString s  -> LS s
    HPR.IChar c    -> LC c

  HPR.PWild          -> Ok $ AST.PWild

  HPR.PTuple [p]     -> transformPatTuple p
  HPR.PTuple ps      -> do ps' <- mapM transformPatTuple ps
                           Ok $ AST.PTuple ps'

transformPatTuple :: HPR.PatTuple -> Err AST.Pattern
transformPatTuple a = case a of
  HPR.PTCon (IdCon s) qs -> do qs' <- mapM transformPat qs
                               Ok $ AST.PCon s qs'
  HPR.PTPat p            -> transformPat p

transformArg :: HPR.Arg -> Err AST.Pattern
transformArg (APat p) = transformPat p

transformClause :: HPR.Clause -> Err (AST.Pattern, AST.Expression)
transformClause (CClause pat e) = case pat of
  CCPPat p -> do p' <- transformPat p
                 e' <- transformExpr e
                 Ok (p',e')

  CCPCon (IdCon i) p -> do p' <- mapM transformPat p
                           let p'' = AST.PCon i p'
                           e' <- transformExpr e
                           Ok (p'',e')


--
-- Helper functions
--

-- | Take out the identifier from an Id
fromId :: Id -> Identifier
fromId (ICon (IdCon s)) = s
fromId (IVar (IdVar s)) = s

-- | Find ADT declarations and take them out to an own map
findADTs :: Modulename -> [HPR.Def] -> (M.Map Identifier AST.Type, [HPR.Def])
findADTs name defs = foldr go (M.empty,[]) defs -- reverse? 
  where 
    go :: HPR.Def 
       -> (M.Map Identifier AST.Type,[HPR.Def]) 
       -> (M.Map Identifier AST.Type,[HPR.Def])

    -- Put ADT in map
    go d (m,ds) = case d of 
      DAdt (AAdt (IdCon t) vars cons) -> 
        let m' = M.fromList $ map (dataToSignature name t vars) cons
        in (m <> m', ds)
      
      -- Otherwise skip
      d' -> (m,d':ds)


-- | Convert a data constructor to signature
--   The second argument is the last part of the signature
dataToSignature :: Modulename -> Constructor -> [AdtVar] 
                -> AdtCon -> (Constructor, AST.Type)
dataToSignature name ty vars cons = undefined

-- | A temporary expression representing a function without an expression yet
eundefined :: Expression
eundefined = AST.EVar "undefined" 

-- | Check that there are no signatures without function definitions
checkLonelySignatures :: Function (Maybe AST.Type) -> Err ()
checkLonelySignatures (Fun name i (Just s) e) = case e == eundefined of
  True -> Bad $ "Lonley signature '" ++ name ++ "." ++ i 
                ++ " :: " ++ show s ++ "'" 
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
