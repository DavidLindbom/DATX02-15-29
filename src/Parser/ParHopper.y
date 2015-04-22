-- This Happy file was machine-generated by the BNF converter
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module Parser.ParHopper where
import Parser.AbsHopper
import Parser.LexHopper
import Utils.ErrM

}

%name pModule Module

-- no lexer declaration
%monad { Err } { thenM } { returnM }
%tokentype { Token }

%token
  '(' { PT _ (TS _ 1) }
  ')' { PT _ (TS _ 2) }
  ',' { PT _ (TS _ 3) }
  '->' { PT _ (TS _ 4) }
  '::' { PT _ (TS _ 5) }
  ';' { PT _ (TS _ 6) }
  '=' { PT _ (TS _ 7) }
  '\\' { PT _ (TS _ 8) }
  '_' { PT _ (TS _ 9) }
  'case' { PT _ (TS _ 10) }
  'data' { PT _ (TS _ 11) }
  'else' { PT _ (TS _ 12) }
  'if' { PT _ (TS _ 13) }
  'import' { PT _ (TS _ 14) }
  'module' { PT _ (TS _ 15) }
  'of' { PT _ (TS _ 16) }
  'then' { PT _ (TS _ 17) }
  'where' { PT _ (TS _ 18) }
  '{' { PT _ (TS _ 19) }
  '|' { PT _ (TS _ 20) }
  '}' { PT _ (TS _ 21) }

L_integ  { PT _ (TI $$) }
L_doubl  { PT _ (TD $$) }
L_quoted { PT _ (TL $$) }
L_charac { PT _ (TC $$) }
L_IdVar { PT _ (T_IdVar $$) }
L_IdCon { PT _ (T_IdCon $$) }
L_IdOpr { PT _ (T_IdOpr $$) }


%%

Integer :: { Integer } : L_integ  { (read ( $1)) :: Integer }
Double  :: { Double }  : L_doubl  { (read ( $1)) :: Double }
String  :: { String }  : L_quoted {  $1 }
Char    :: { Char }    : L_charac { (read ( $1)) :: Char }
IdVar    :: { IdVar} : L_IdVar { IdVar ($1)}
IdCon    :: { IdCon} : L_IdCon { IdCon ($1)}
IdOpr    :: { IdOpr} : L_IdOpr { IdOpr ($1)}

Module :: { Module }
Module : 'module' IdCon Exports 'where' ';' ListImport ListDef { MMod $2 $3 $6 $7 } 


Exports :: { Exports }
Exports : {- empty -} { NEmpty } 
  | '(' ListExport ')' { NExps $2 }


Export :: { Export }
Export : Id { NExp $1 } 


ListExport :: { [Export] }
ListExport : {- empty -} { [] } 
  | Export { (:[]) $1 }
  | Export ',' ListExport { (:) $1 $3 }


Import :: { Import }
Import : 'import' IdCon { IImport $2 } 


ListImport :: { [Import] }
ListImport : {- empty -} { [] } 
  | Import { (:[]) $1 }
  | Import ';' ListImport { (:) $1 $3 }


Def :: { Def }
Def : Func { DFun $1 } 
  | Sign { DSig $1 }
  | Adt { DAdt $1 }


ListDef :: { [Def] }
ListDef : {- empty -} { [] } 
  | Def { (:[]) $1 }
  | Def ';' ListDef { (:) $1 $3 }


Func :: { Func }
Func : IdVar ListArg '=' '{' Expr '}' { FFun $1 (reverse $2) $5 } 


Arg :: { Arg }
Arg : Pat { APat $1 } 


ListArg :: { [Arg] }
ListArg : {- empty -} { [] } 
  | ListArg Arg { flip (:) $1 $2 }


Expr2 :: { Expr }
Expr2 : Id { EId $1 } 
  | Prim { EPrim $1 }
  | '(' IdOpr ')' { EOpr $2 }
  | '(' Expr ')' { $2 }


Expr1 :: { Expr }
Expr1 : Expr1 IdOpr Expr2 { EInfix $1 $2 $3 } 
  | Expr1 Expr2 { EApp $1 $2 }
  | 'case' Expr1 'of' '{' ListClause '}' { ECase $2 $5 }
  | 'if' Expr1 'then' Expr2 'else' Expr2 { EIf $2 $4 $6 }
  | Expr2 { $1 }


Expr :: { Expr }
Expr : '\\' ListPat '->' Expr { ELambda $2 $4 } 
  | Expr1 { $1 }


Clause :: { Clause }
Clause : ClausePat '->' Expr { CClause $1 $3 } 


ListClause :: { [Clause] }
ListClause : Clause { (:[]) $1 } 
  | Clause ';' ListClause { (:) $1 $3 }


ClausePat :: { ClausePat }
ClausePat : Pat { CCPPat $1 } 
  | IdCon ListPat { CCPCon $1 $2 }


Pat :: { Pat }
Pat : Id { PId $1 } 
  | Prim { PPrim $1 }
  | '_' { PWild }
  | '(' ListPatTuple ')' { PTuple $2 }


ListPat :: { [Pat] }
ListPat : Pat { (:[]) $1 } 
  | Pat ListPat { (:) $1 $2 }


PatTuple :: { PatTuple }
PatTuple : IdCon ListPat { PTCon $1 $2 } 
  | Pat { PTPat $1 }


ListPatTuple :: { [PatTuple] }
ListPatTuple : {- empty -} { [] } 
  | PatTuple { (:[]) $1 }
  | PatTuple ',' ListPatTuple { (:) $1 $3 }


Sign :: { Sign }
Sign : IdVar '::' '{' ListType '}' { SSig $1 $4 } 


Type :: { Type }
Type : IdCon ListTypeArg { TName $1 (reverse $2) } 
  | IdVar { TVar $1 }
  | '(' ListTypeTuple ')' { TTuple $2 }


ListType :: { [Type] }
ListType : Type { (:[]) $1 } 
  | Type '->' ListType { (:) $1 $3 }


TypeTuple :: { TypeTuple }
TypeTuple : ListType { TTTuple $1 } 


ListTypeTuple :: { [TypeTuple] }
ListTypeTuple : {- empty -} { [] } 
  | TypeTuple { (:[]) $1 }
  | TypeTuple ',' ListTypeTuple { (:) $1 $3 }


TypeArg :: { TypeArg }
TypeArg : Id { TTAId $1 } 
  | '(' ListTypeTuple ')' { TTATuple $2 }


ListTypeArg :: { [TypeArg] }
ListTypeArg : {- empty -} { [] } 
  | ListTypeArg TypeArg { flip (:) $1 $2 }


Adt :: { Adt }
Adt : 'data' IdCon ListAdtVar '=' '{' ListAdtCon '}' { AAdt $2 (reverse $3) $6 } 


AdtVar :: { AdtVar }
AdtVar : IdVar { AVVar $1 } 


ListAdtVar :: { [AdtVar] }
ListAdtVar : {- empty -} { [] } 
  | ListAdtVar AdtVar { flip (:) $1 $2 }


AdtCon :: { AdtCon }
AdtCon : IdCon ListAdtArg { ACCon $1 (reverse $2) } 


ListAdtCon :: { [AdtCon] }
ListAdtCon : AdtCon { (:[]) $1 } 
  | AdtCon '|' ListAdtCon { (:) $1 $3 }


AdtArg :: { AdtArg }
AdtArg : Id { AAId $1 } 
  | '(' ListAdtArgTuple ')' { AATuple $2 }


ListAdtArg :: { [AdtArg] }
ListAdtArg : {- empty -} { [] } 
  | ListAdtArg AdtArg { flip (:) $1 $2 }


AdtArgTuple :: { AdtArgTuple }
AdtArgTuple : IdCon AdtArg ListAdtArg { AATCon $1 $2 (reverse $3) } 
  | AdtArg { AATArg $1 }


ListAdtArgTuple :: { [AdtArgTuple] }
ListAdtArgTuple : AdtArgTuple { (:[]) $1 } 
  | AdtArgTuple ',' ListAdtArgTuple { (:) $1 $3 }


Id :: { Id }
Id : IdCon { ICon $1 } 
  | IdVar { IVar $1 }


ListId :: { [Id] }
ListId : {- empty -} { [] } 
  | ListId Id { flip (:) $1 $2 }


Prim :: { Prim }
Prim : Integer { IInteger $1 } 
  | Double { IDouble $1 }
  | String { IString $1 }
  | Char { IChar $1 }



{

returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    _ -> " before " ++ unwords (map (id . prToken) (take 4 ts))

myLexer = tokens
}

