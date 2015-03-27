module Renamer.Renamer (transform) where

import qualified Data.Map as M
import Control.Monad 
import Data.Monoid

import Parser.AbsHopper as HPR
import AST.AST as AST
import Utils.ErrM
import Utils.PrettyPrint

transform :: HPR.Module -> Err (AST.Module (Maybe Signature))
transform (MModule (IdCon name) expo defs) = do
  let (adts,defs') = findADTs defs
  defs'' <- transformDefs defs'
  mapM_ checkLonelySignatures defs''
  expo' <- case expo of
            [] -> let e1 = map (\(Fun n _ _) -> n) defs''
                      e2 = M.keys adts
                  in return (e1++e2)
            _  -> transformExports expo
  -- TODO: Should be moved to after dependency resolver
  --checkExports expo' defs''
  return $ Mod name expo' defs'' adts

--
-- All transform* functions is a transform from the parse tree to AST
--

transformExports :: [Export] -> Err [Identifier]
transformExports exps = Ok $ map (\(MExport (IdVar s)) -> s) exps
 
transformDefs :: [Def] -> Err [Function (Maybe Signature)]
transformDefs defs = do
  funs <- foldM go M.empty defs 
  return $ M.elems funs
  where
    go :: M.Map Identifier (Function (Maybe Signature)) -> Def
       -> Err (M.Map Identifier (Function (Maybe Signature)))
   
    go m (DSig (IdVar i) t) = case M.lookup i m of
        -- There is no function
        Nothing -> 
          return $ M.insert i (Fun i (Just $ transformTypes t) eundefined) m
        
        -- There is a function, but no signature
        Just (Fun _ Nothing e) -> 
          return $ M.insert i (Fun i (Just $ transformTypes t) e) m
        
        -- There is a function and a signature
        _ -> fail $ "Multiple signatures for '" ++ i ++ "'"
    
    go m (DFun (IdVar i) as e) = do
      e' <- transformExp e

      -- Add lambda if arguments
      e'' <- if null as
              then return e'
              else do pat <- mapM transformArg as
                      return $ AST.ELambda pat e'     

      case M.lookup i m of
        Nothing           -> return $ M.insert i (Fun i Nothing e'') m
        Just (Fun _ t es) -> do
          cs <- mergeCase es e''
          return $ M.insert i (Fun i t cs) m

    go _ (DDat (IdCon s) _) = fail $ "Bug! Found data declaration '" 
                                     ++ s ++ "' in transformDefs"

transformTypes :: [HPR.Type] -> Signature
transformTypes = map go
  where
    go :: HPR.Type -> AST.Type
    go (HPR.TName (IdCon a)) = AST.TName a []
    go (HPR.TVar  (IdVar a)) = AST.TVar a
    go (HPR.TFun t ts)       = AST.TFun (go t:map go ts)


-- Questions: 
-- * Should we add some type inference here? At least on literals?
-- * Should EOpr be special in some way?
transformExp :: Exp -> Err Expression
transformExp e = case e of
  HPR.EVar (IdVar i) -> Ok $ AST.EVar i
  HPR.ECon (IdCon i) -> Ok $ AST.ECon i
  EOpr (IdOpr i)     -> Ok $ AST.EVar i

  EString s          -> Ok $ ELit $ LS s
  EChar c            -> Ok $ ELit $ LC c
  EInteger i         -> Ok $ ELit $ LI i
  EDouble d          -> Ok $ ELit $ LD d

  EInfix a op b      -> (transformExp $ EOpr op) 
                        `app` (transformExp a) 
                        `app` (transformExp b)

  HPR.EApp a b       -> (transformExp a) `app` (transformExp b)

  HPR.ECase a c      -> do e' <- transformExp a
                           c' <- mapM transformClause c
                           Ok $ AST.ECase e' c'
  
  HPR.EIf a b c      -> do a' <- transformExp a
                           b' <- transformExp b
                           c' <- transformExp c
                           Ok $ AST.ECase a' [(AST.PCon "True"  [], b')
                                             ,(AST.PCon "False" [], c')]

  HPR.ELambda ps a   -> do a'  <- transformExp a 
                           ps' <- mapM transformPat ps 
                           Ok $ AST.ELambda ps' a' 
                           
  where app (Bad m) _      = Bad m
        app _      (Bad m) = Bad m 
        app (Ok a) (Ok b)  = Ok $ AST.EApp a b

transformPat :: Pat -> Err Pattern
transformPat p = case p of
  HPR.PCon (IdCon i) -> Ok $ AST.PCon i []
  HPR.PVar (IdVar i) -> Ok $ AST.PVar i
  HPR.PWild          -> Ok $ AST.PWild
  HPR.PString s      -> Ok $ AST.PLit $ LS s
  HPR.PChar c        -> Ok $ AST.PLit $ LC c
  HPR.PInteger i     -> Ok $ AST.PLit $ LI i
  HPR.PDouble d      -> Ok $ AST.PLit $ LD d

  HPR.PTuple [p]     -> transformQpat p
  HPR.PTuple ps      -> do ps' <- mapM transformQpat ps
                           Ok $ AST.PTuple ps'

transformQpat :: Qpat -> Err Pattern
transformQpat a = case a of
  QCon (IdCon s) qs -> do qs' <- mapM transformQpat qs
                          Ok $ AST.PCon s qs'
  QPat p            -> transformPat p

transformArg :: Arg -> Err Pattern
transformArg a = case a of
  ACon (IdCon i) -> Ok $ AST.PCon i []
  AVar (IdVar i) -> Ok $ AST.PVar i
  AWild          -> Ok $ AST.PWild
  AString s      -> Ok $ AST.PLit $ LS s
  AChar c        -> Ok $ AST.PLit $ LC c
  AInteger i     -> Ok $ AST.PLit $ LI i
  ADouble d      -> Ok $ AST.PLit $ LD d

  ATuple [b]     -> transformBarg b
  ATuple bs      -> do bs' <- mapM transformBarg bs
                       Ok $ AST.PTuple bs'

transformBarg :: Barg -> Err Pattern
transformBarg a = case a of
  BCon (IdCon s) as -> do as' <- mapM transformArg as
                          Ok $ AST.PCon s as'
  BArg a            -> transformArg a                        

transformClause :: Cla -> Err (Pattern, Expression)
transformClause c = case c of
  CClause p e -> do p' <- transformPat p
                    e' <- transformExp e
                    Ok (p',e')

--
-- Helper functions
--

-- | Find ADT declarations and take them out to an own map
findADTs :: [Def] -> (M.Map Identifier Signature, [Def])
findADTs defs = foldr go (M.empty,[]) defs -- reverse? 
  where 
    go :: Def -> (M.Map Identifier Signature,[Def]) -> (M.Map Identifier Signature,[Def])
    go d (m,ds) = case d of 
      
      DDat (IdCon t) cons -> let m' = M.fromList $ map (dataToSignature t) cons
                             in (m <> m', ds)
      
      d' -> (m,d':ds)


-- | Convert a data constructor to signature
--   The first argument is the last part of the signature
--   
dataToSignature :: Identifier -> Cons -> (Identifier, Signature)
dataToSignature ty cons = case cons of
  FCon (IdCon c) cs -> (c, map go cs ++ [AST.TName ty []])
  where 
    go :: Par -> AST.Type
    go (GCon (IdCon s)) = AST.TName s []

-- | A temporary expression representing a function without an expression yet
eundefined :: Expression
eundefined = AST.EVar "undefined" 

-- | Check that there are no signatures without function definitions
checkLonelySignatures :: Function (Maybe Signature) -> Err ()
checkLonelySignatures (Fun i (Just s) e) = case e == eundefined of
  True -> Bad $ "Lonley signature '" ++ i ++ " :: " ++ showSignature s ++ "'" 
  _    -> Ok ()
checkLonelySignatures _ = Ok ()

-- | Check that there is a definition for all exported functions
checkExports :: [Identifier] -> [Function a] -> Err ()
checkExports ids fs = sequence_ $ map exists ids
  where exists i = if any (\(Fun f _ _) -> f == i) fs
                  then Ok ()
                  else Bad $ "Undefined export '" ++ i ++ "'"

-- | Convert two pattern matching functions to a case expression
-- TODO: How should the signatures be handled here?
mergeCase :: Expression -> Expression -> Err Expression
mergeCase a b = case (a,b) of

  -- Replace eundefined with expression
  (c,d) | c == eundefined -> Ok d

  -- Add new clause to case 
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



