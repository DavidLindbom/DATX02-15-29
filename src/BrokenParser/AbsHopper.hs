

module Parser.AbsHopper where

-- Haskell module generated by the BNF converter




newtype TIdVar = TIdVar String deriving (Eq,Ord,Show,Read)
newtype TIdCon = TIdCon String deriving (Eq,Ord,Show,Read)
newtype IdOpr = IdOpr String deriving (Eq,Ord,Show,Read)
newtype TQIdVar = TQIdVar String deriving (Eq,Ord,Show,Read)
newtype TQIdCon = TQIdCon String deriving (Eq,Ord,Show,Read)
data Module =
   MMod IdCon Exports [Import] [Def]
  deriving (Eq,Ord,Show,Read)

data Exports =
   NEmpty
 | NExps [Export]
  deriving (Eq,Ord,Show,Read)

data Export =
   NExp Id
  deriving (Eq,Ord,Show,Read)

data Import =
   IImport IdCon
  deriving (Eq,Ord,Show,Read)

data Def =
   DFun Func
 | DSig Sign
 | DAdt Adt
  deriving (Eq,Ord,Show,Read)

data Func =
   FFun TIdVar [Arg] Expr
  deriving (Eq,Ord,Show,Read)

data Arg =
   APat Pat
  deriving (Eq,Ord,Show,Read)

data Expr =
   EId Id
 | EPrim Prim
 | EOpr IdOpr
 | EInfix Expr IdOpr Expr
 | EApp Expr Expr
 | ECase Expr [Clause]
 | EIf Expr Expr Expr
 | ELambda [Pat] Expr
  deriving (Eq,Ord,Show,Read)

data Clause =
   CClause ClausePat Expr
  deriving (Eq,Ord,Show,Read)

data ClausePat =
   CCPPat Pat
 | CCPCon IdCon [Pat]
  deriving (Eq,Ord,Show,Read)

data Pat =
   PCon IdCon
 | PVar TIdVar
 | PPrim Prim
 | PWild
 | PTuple [PatTuple]
  deriving (Eq,Ord,Show,Read)

data PatTuple =
   PTCon IdCon [Pat]
 | PTPat Pat
  deriving (Eq,Ord,Show,Read)

data Sign =
   SSig IdVar [Type]
  deriving (Eq,Ord,Show,Read)

data Type =
   TName IdCon [TypeArg]
 | TVar TIdVar
 | TTuple [TypeTuple]
  deriving (Eq,Ord,Show,Read)

data TypeTuple =
   TTTuple [Type]
  deriving (Eq,Ord,Show,Read)

data TypeArg =
   TTAId Id
 | TTATuple [TypeTuple]
  deriving (Eq,Ord,Show,Read)

data Adt =
   AAdt TIdCon [AdtVar] [AdtCon]
  deriving (Eq,Ord,Show,Read)

data AdtVar =
   AVVar TIdVar
  deriving (Eq,Ord,Show,Read)

data AdtCon =
   ACCon TIdCon [AdtArg]
  deriving (Eq,Ord,Show,Read)

data AdtArg =
   AAId IdCon
 | AAVar TIdVar
 | AATuple [AdtArgTuple]
  deriving (Eq,Ord,Show,Read)

data AdtArgTuple =
   AATCon IdCon AdtArg [AdtArg]
 | AATArg AdtArg
  deriving (Eq,Ord,Show,Read)

data IdVar =
   IdVarNQ TIdVar
 | IdVarQ TQIdVar
  deriving (Eq,Ord,Show,Read)

data IdCon =
   IdConNQ TIdCon
 | IdConQ TQIdCon
  deriving (Eq,Ord,Show,Read)

data Id =
   ICon IdCon
 | IVar IdVar
  deriving (Eq,Ord,Show,Read)

data Prim =
   IInteger Integer
 | IDouble Double
 | IString String
 | IChar Char
  deriving (Eq,Ord,Show,Read)
