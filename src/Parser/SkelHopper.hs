module Parser.SkelHopper where

-- Haskell module generated by the BNF converter

import Parser.AbsHopper
import Parser.ErrM
type Result = Err String

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

transIdVar :: IdVar -> Result
transIdVar x = case x of
  IdVar str  -> failure x


transIdCon :: IdCon -> Result
transIdCon x = case x of
  IdCon str  -> failure x


transIdOpr :: IdOpr -> Result
transIdOpr x = case x of
  IdOpr str  -> failure x


transModule :: Module -> Result
transModule x = case x of
  MModule idcon exports defs  -> failure x


transExport :: Export -> Result
transExport x = case x of
  MExport idvar  -> failure x


transDef :: Def -> Result
transDef x = case x of
  DSig idvar types  -> failure x
  DFun idvar exp  -> failure x
  DCollected idvar def defs  -> failure x


transType :: Type -> Result
transType x = case x of
  TName idcon  -> failure x
  TVar idvar  -> failure x


transExp :: Exp -> Result
transExp x = case x of
  EVar idvar  -> failure x
  ECon idcon  -> failure x
  EOpr idopr  -> failure x
  EString str  -> failure x
  EChar c  -> failure x
  EInteger n  -> failure x
  EDouble d  -> failure x
  EInfix exp1 idopr2 exp3  -> failure x
  EApp exp1 exp2  -> failure x
  ELambda pats exp  -> failure x


transPat :: Pat -> Result
transPat x = case x of
  PCon idcon  -> failure x
  PVar idvar  -> failure x
  PWild  -> failure x



