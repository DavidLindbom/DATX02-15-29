module Renamer.Renamer (transform) where

import qualified Data.Map as M
import Control.Monad 

import Parser.AbsHopper as HPR
import AST.AST as AST
import Parser.ErrM --Utils.ErrM
import Utils.PrettyPrint

transform :: HPR.Module -> Err (AST.Module (Maybe Signature))
transform (MModule (IdCon name) expo defs) = do
  defs' <- transformDefs defs
  mapM_ checkLonelySignatures defs'
  expo' <- case expo of
            [] -> return $ map (\(Fun n _ _) -> n) defs'
            _  -> transformExports expo
  checkExports expo' defs'
  return $ Mod name expo' defs'

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
                      return $ AST.ELambda Nothing pat e'     

      case M.lookup i m of
        Nothing           -> return $ M.insert i (Fun i Nothing e'') m
        Just (Fun _ t es) -> do
          cs <- mergeCase es e''
          return $ M.insert i (Fun i t cs) m


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
transformExp :: Exp -> Err (Expression (Maybe Signature))
transformExp e = case e of
  HPR.EVar (IdVar i) -> Ok $ AST.EVar Nothing i
  HPR.ECon (IdCon i) -> Ok $ AST.ECon Nothing i
  EOpr (IdOpr i)     -> Ok $ AST.EVar Nothing i

  EString s          -> Ok $ ELit (Just [AST.TName "String"  []]) $ LS s
  EChar c            -> Ok $ ELit (Just [AST.TName "Char"    []]) $ LC c
  EInteger i         -> Ok $ ELit (Just [AST.TName "Integer" []]) $ LI i
  EDouble d          -> Ok $ ELit (Just [AST.TName "Double"  []]) $ LD d

  EInfix a op b      -> (transformExp $ EOpr op) 
                        `app` (transformExp a) 
                        `app` (transformExp b)

  HPR.EApp a b       -> (transformExp a) `app` (transformExp b)
  HPR.ELambda ps a   -> do a'  <- transformExp a 
                           ps' <- mapM transformPat ps 
                           return $ AST.ELambda Nothing ps' a' 
                           
  where app (Bad m) _      = Bad m
        app _      (Bad m) = Bad m 
        app (Ok a) (Ok b)  = Ok $ AST.EApp Nothing a b

transformPat :: Pat -> Err Pattern
transformPat p = case p of
  HPR.PCon (IdCon i) -> Ok $ AST.PCon i
  HPR.PVar (IdVar i) -> Ok $ AST.PVar i
  HPR.PWild          -> Ok AST.PWild
  -- TODO add literal pattern


transformArg :: Arg -> Err Pattern
transformArg a = case a of
  ACon (IdCon i) -> Ok $ AST.PCon i
  AVar (IdVar i) -> Ok $ AST.PCon i
  AWild          -> Ok $ AST.PWild

--
-- Helper functions
--

-- | A temporary expression representing a function without an expression yet
eundefined = AST.EVar Nothing "undefined" 

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
--mergeCase :: Expression a -> Expression a -> Err (Expression a)
mergeCase a b = case a of
  
  -- Add a new clause to case
  ECase t c -> case b of
    AST.ELambda _ p e -> Ok $ AST.ECase t (c++[(p,e)])

  e -> Bad $ "undefined case in 'mergeCase': " ++ show e



