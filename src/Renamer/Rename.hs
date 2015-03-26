
module Renamer.Rename where

import qualified Parser.AbsHopper as A --maybe not qualify it?
import AST.AST
import TypeChecker.TC
import Data.List (sortBy,groupBy,break)
import Data.Char (isUpper)
import Control.Arrow ((***))
import qualified Data.Set as S

rename :: A.Module -> RenamedModule
rename (A.MModule idcon exps ds) = 
    RenamedModule{modId=case idconToName idcon of
                          Name (Just ms) s -> ms++[s]
                          Name Nothing s -> [s],
                  exports=(map (\(A.MExport(A.IdVar s))->name s) exps),
                  cons=constructors,
                  defs=definitions}
    where (constructors,definitions) = renameDefs ds
--note that rename considers the module in isolation:
--it doesn't expand imported names to their fully qualified versions
--(That can be done in the simpler AST datatype).
--plz add imports <3
    

renameDefs :: [A.Def] -> ([(Name,TypeAST)],[(Name,AST,Maybe TypeAST)])
renameDefs defs = (handleDataDecls $ filter (\def -> case def of
                                                      A.DDat _ _ -> True
                                                      _ -> False) defs,
                  (sortDefsByName ==>
                   groupTogether ==>
                   map renameDef) $ filter (\def -> case def of
                                                      A.DDat _ _ -> False
                                                      _ -> True) defs)
    where
      sortDefsByName = sortBy (\d1 d2 -> 
                                   case compare (nameOf d1) (nameOf d2) of
                                     EQ -> case (d1,d2) of
                                             (A.DSig _ _,A.DFun _ _ _) -> LT
                                             (A.DFun _ _ _,A.DSig _ _) -> GT
                                             (A.DSig id _,A.DSig _ _) ->
                                                 error $"Multiple type sigs"++
                                                        " for "++show id 
                                             _ -> EQ
                                           
                                     o -> o)
      groupTogether = groupBy (\d1 d2 -> nameOf d1 == nameOf d2)
      nameOf (A.DSig id _) = id
      nameOf (A.DFun id _ _) = id
      (==>) f g = g . f

renameDef :: [A.Def] -> (Name,AST,Maybe TypeAST)
renameDef ((A.DSig id ftype):ds) = renameDef' (Just ftype) ds
renameDef ds = renameDef' Nothing ds
renameDef' m [] = error "Type signature without definition"
renameDef' m ds@((A.DFun idvar _ _):_) = (idvarToName idvar,
                                          dsToAST ds,
                                          fmap ftypeToTypeAST m)
    where ftypeToTypeAST ftype = let t = 
                                         foldr1 (\t1 t2 -> 
                                                 t1 `arrowtype` t2) $
                                         map typeToTypeAST ftype
                                 in if tyVarNamesOf t == S.empty
                                    then t
                                    else ForallT t
          typeToTypeAST (A.TName idcon) = ConT $ idconToName idcon
          typeToTypeAST (A.TVar idvar)  = VarT $ idvarToName idvar
          typeToTypeAST (A.TFun t ts) = foldr (\t1 t2 -> AppT t2 t1) 
                                        (typeToTypeAST t) (map typeToTypeAST ts)
          dsToAST ((A.DFun id args b):ds) = let numArgs = length args
                                            in
                                             if not $ and $ map 
                                                    (\(A.DFun _ args _) ->
                                                     length args == numArgs)ds
                                             then error $ 
                                                      "Different numbers "++
                                                      "of arguments in "++
                                                      show id
                                             else {-if numArgs == 0
                                                  then if ds == []
                                                       then expToAST b
                                                       else error $
                                                            "Unreachable case"
                                                            ++" in "++show id
                                                  else -}
                                                      LamAST (newVars numArgs)
                                                  $ cases ((args,b):
                                                           (map
                                                            (\(A.DFun _ args b)
                                                           -> (args,b)) ds))
                                                           numArgs
          newVars numArgs= 
                      [VarPat $ name $ "X@"++show i | i <- [1..numArgs]]
          cases args'bs numArgs = CaseAST (TupleAST $ map
                                            (\(VarPat nm)->
                                             Named nm)
                                          $ newVars numArgs)
                                  $ map ((TuplePat . zipWith AsPat 
                                                   (newVars numArgs) .
                                                   map argToPatAST) *** 
                                         expToAST) args'bs
          argToPatAST (A.ACon idcon) = ConPat $ idconToName idcon
          argToPatAST (A.AVar idvar) = VarPat $ idvarToName idvar
          argToPatAST A.AWild = WildPat
          argToPatAST (A.AString s) = LitPat $ StringL s
          argToPatAST (A.AChar c) = LitPat $ CharL c
          argToPatAST (A.AInteger i) = LitPat $ IntegerL i
          argToPatAST (A.ADouble d) = LitPat $ DoubleL d
          --Plz add mo' cases <3
idconToName (A.IdCon s) = strToName s
idvarToName (A.IdVar s) = strToName s
idoprToName (A.IdOpr s) = strToName s
strToName (c:s) | isUpper c = case break (=='.') (c:s) of
                                (con,"") -> name con
                                (mod,'.':s') -> addModule mod $ strToName s'
                | otherwise = Name Nothing $ c:s 
                where
                  addModule m (Name Nothing s) = Name (Just[m]) s
                  addModule m (Name (Just ms) s) = Name (Just$m:ms) s
handleDataDecls :: [A.Def] -> [(Name,TypeAST)]
handleDataDecls = (>>= \(A.DDat idcon cons)-> map (handleCon idcon) cons)
                  where handleCon dt (A.FCon idcon pars) = 
                            (idconToName idcon,
                            foldr arrowtype (ConT $ idconToName idcon) $
                             map (\(A.GCon idcon)->ConT$idconToName idcon) 
                             pars)

expToAST :: A.Exp -> AST
expToAST (A.EVar idvar) = Named $ idvarToName idvar
expToAST (A.ECon idcon) = Named $ idconToName idcon
expToAST (A.EOpr idopr) = Named $ idoprToName idopr
expToAST (A.EString s) = LitAST $ StringL s
expToAST (A.EChar c) = LitAST $ CharL c
expToAST (A.EInteger i) = LitAST $ IntegerL i
expToAST (A.EDouble d) = LitAST $ DoubleL d
expToAST (A.EInfix e1 idopr e2) = expToAST (A.EOpr idopr) `AppAST`
                                  (expToAST e1) `AppAST` 
                                  (expToAST e2)
--A case needs to be added for (+ 3) and (3 +):
--EInfix' (Maybe Exp) IdOpr (Maybe Exp)
--also, note that prefix functions can be 
--used as infix with ``
expToAST (A.EApp f x) = expToAST f `AppAST` expToAST x
expToAST (A.ECase e clauses) = CaseAST (expToAST e) $
                             map (\(A.CClause pat exp)->(patToAST pat,
                                                         expToAST exp))
                             clauses
expToAST (A.EIf i t e) = IfAST (expToAST i) (expToAST t) (expToAST e)
expToAST (A.ELambda pats body) = LamAST (map patToAST pats) $ expToAST body

patToAST (A.PCon idcon) = ConPat $ idconToName idcon
patToAST (A.PVar idvar) = VarPat $ idvarToName idvar
patToAST A.PWild = WildPat
patToAST (A.PString s) = LitPat $ StringL s
patToAST (A.PChar c) = LitPat $ CharL c
patToAST (A.PInteger i) = LitPat $ IntegerL i
patToAST (A.PDouble d) = LitPat $ DoubleL d
