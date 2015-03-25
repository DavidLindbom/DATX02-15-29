{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module Parser.PrintHopper where

-- pretty-printer generated by the BNF converter

import Parser.AbsHopper
import Data.Char


-- the top-level printing method
printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 (map ($ "") $ d []) "" where
  rend i ss = case ss of
    "["      :ts -> showChar '[' . rend i ts
    "("      :ts -> showChar '(' . rend i ts
    "{"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts
    "}" : ";":ts -> new (i-1) . space "}" . showChar ';' . new (i-1) . rend (i-1) ts
    "}"      :ts -> new (i-1) . showChar '}' . new (i-1) . rend (i-1) ts
    ";"      :ts -> showChar ';' . new i . rend i ts
    t  : "," :ts -> showString t . space "," . rend i ts
    t  : ")" :ts -> showString t . showChar ')' . rend i ts
    t  : "]" :ts -> showString t . showChar ']' . rend i ts
    t        :ts -> space t . rend i ts
    _            -> id
  new i   = showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
  space t = showString t . (\s -> if null s then "" else (' ':s))

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- the printer class does the job
class Print a where
  prt :: Int -> a -> Doc
  prtList :: [a] -> Doc
  prtList = concatD . map (prt 0)

instance Print a => Print [a] where
  prt _ = prtList

instance Print Char where
  prt _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')
  prtList s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q s = case s of
  _ | s == q -> showChar '\\' . showChar s
  '\\'-> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  _ -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j<i then parenth else id


instance Print Integer where
  prt _ x = doc (shows x)


instance Print Double where
  prt _ x = doc (shows x)



instance Print IdVar where
  prt _ (IdVar i) = doc (showString ( i))


instance Print IdCon where
  prt _ (IdCon i) = doc (showString ( i))


instance Print IdOpr where
  prt _ (IdOpr i) = doc (showString ( i))



instance Print Module where
  prt i e = case e of
   MModule idcon exports defs -> prPrec i 0 (concatD [doc (showString "module") , prt 0 idcon , doc (showString "(") , prt 0 exports , doc (showString ")") , doc (showString "where") , doc (showString ";") , prt 0 defs])


instance Print Export where
  prt i e = case e of
   MExport idvar -> prPrec i 0 (concatD [prt 0 idvar])

  prtList es = case es of
   [] -> (concatD [])
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , doc (showString ",") , prt 0 xs])

instance Print Def where
  prt i e = case e of
   DSig idvar types -> prPrec i 0 (concatD [prt 0 idvar , doc (showString "::") , prt 0 types])
   DFun idvar args exp -> prPrec i 0 (concatD [prt 0 idvar , prt 0 args , doc (showString "=") , doc (showString "{") , prt 0 exp , doc (showString "}")])
   DDat idcon conss -> prPrec i 0 (concatD [doc (showString "data") , prt 0 idcon , doc (showString "=") , doc (showString "{") , prt 0 conss , doc (showString "}")])

  prtList es = case es of
   [] -> (concatD [])
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , doc (showString ";") , prt 0 xs])

instance Print Cons where
  prt i e = case e of
   FCon idcon pars -> prPrec i 0 (concatD [prt 0 idcon , prt 0 pars])

  prtList es = case es of
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , doc (showString "|") , prt 0 xs])

instance Print Par where
  prt i e = case e of
   GCon idcon -> prPrec i 0 (concatD [prt 0 idcon])

  prtList es = case es of
   [] -> (concatD [])
   x:xs -> (concatD [prt 0 x , prt 0 xs])

instance Print Arg where
  prt i e = case e of
   ACon idcon -> prPrec i 0 (concatD [prt 0 idcon])
   AVar idvar -> prPrec i 0 (concatD [prt 0 idvar])
   AWild  -> prPrec i 0 (concatD [doc (showString "_")])
   AString str -> prPrec i 0 (concatD [prt 0 str])
   AChar c -> prPrec i 0 (concatD [prt 0 c])
   AInteger n -> prPrec i 0 (concatD [prt 0 n])
   ADouble d -> prPrec i 0 (concatD [prt 0 d])
   ATuple bargs -> prPrec i 0 (concatD [doc (showString "(") , prt 0 bargs , doc (showString ")")])

  prtList es = case es of
   [] -> (concatD [])
   [] -> (concatD [])
   x:xs -> (concatD [prt 0 x , prt 0 xs])
   x:xs -> (concatD [prt 0 x , prt 0 xs])

instance Print Barg where
  prt i e = case e of
   BCon idcon args -> prPrec i 0 (concatD [prt 0 idcon , prt 0 args])
   BArg arg -> prPrec i 0 (concatD [prt 0 arg])

  prtList es = case es of
   [] -> (concatD [])
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , doc (showString ",") , prt 0 xs])

instance Print Type where
  prt i e = case e of
   TName idcon -> prPrec i 0 (concatD [prt 0 idcon])
   TVar idvar -> prPrec i 0 (concatD [prt 0 idvar])
   TFun type' types -> prPrec i 0 (concatD [doc (showString "(") , prt 0 type' , doc (showString "->") , prt 0 types , doc (showString ")")])

  prtList es = case es of
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , doc (showString "->") , prt 0 xs])

instance Print Exp where
  prt i e = case e of
   EVar idvar -> prPrec i 2 (concatD [prt 0 idvar])
   ECon idcon -> prPrec i 2 (concatD [prt 0 idcon])
   EOpr idopr -> prPrec i 2 (concatD [doc (showString "(") , prt 0 idopr , doc (showString ")")])
   EString str -> prPrec i 2 (concatD [prt 0 str])
   EChar c -> prPrec i 2 (concatD [prt 0 c])
   EInteger n -> prPrec i 2 (concatD [prt 0 n])
   EDouble d -> prPrec i 2 (concatD [prt 0 d])
   EInfix exp0 idopr exp -> prPrec i 1 (concatD [prt 1 exp0 , prt 0 idopr , prt 2 exp])
   EApp exp0 exp -> prPrec i 1 (concatD [prt 1 exp0 , prt 2 exp])
   ECase exp clas -> prPrec i 1 (concatD [doc (showString "case") , prt 1 exp , doc (showString "of") , doc (showString "{") , prt 0 clas , doc (showString "}")])
   EIf exp0 exp1 exp -> prPrec i 1 (concatD [doc (showString "if") , prt 1 exp0 , doc (showString "then") , prt 2 exp1 , doc (showString "else") , prt 2 exp])
   ELambda pats exp -> prPrec i 0 (concatD [doc (showString "\\") , prt 0 pats , doc (showString "->") , prt 0 exp])


instance Print Cla where
  prt i e = case e of
   CClause pat exp -> prPrec i 0 (concatD [prt 0 pat , doc (showString "->") , prt 0 exp])

  prtList es = case es of
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , doc (showString ";") , prt 0 xs])

instance Print Pat where
  prt i e = case e of
   PCon idcon -> prPrec i 0 (concatD [prt 0 idcon])
   PVar idvar -> prPrec i 0 (concatD [prt 0 idvar])
   PWild  -> prPrec i 0 (concatD [doc (showString "_")])
   PString str -> prPrec i 0 (concatD [prt 0 str])
   PChar c -> prPrec i 0 (concatD [prt 0 c])
   PInteger n -> prPrec i 0 (concatD [prt 0 n])
   PDouble d -> prPrec i 0 (concatD [prt 0 d])

  prtList es = case es of
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , prt 0 xs])


