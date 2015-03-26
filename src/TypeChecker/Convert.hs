
module TypeChecker.Convert where

--A module containing functions to convert
--to and from the representation of modules that
--the liam-tc-typechecker uses.
--Since grammar does not yet have modules,
--I won't implement "M1.M2.foo" ==> Name ["M1","M2"] foo;
--I will assume there are no module prefixes.
import qualified AST.Liam_TC_AST as L
import qualified AST.AST as A

moduleToRenamed :: A.Module (Maybe A.Type)-> L.RenamedModule
moduleToRenamed (A.Mod s is fs) = L.RenamedModule
                                       [s]
                                       (map idToName is)
                                       [] --constructors not supported
                                       (map functionToName'AST'MType fs)
    where
      splitEveryDot s = case break (=='.') s of
                          (s,"") -> [s]
                          (s,'.':ss) -> s:splitEveryDot ss

idToName :: A.Identifier -> L.Name
idToName s = L.Name Nothing s

functionToName'AST'MType :: A.Function (Maybe A.Type) -> 
    (L.Name,L.AST,Maybe L.TypeAST)
functionToName'AST'MType (A.Fun id mtype expression) = 
    (idToName id,expToAST expression, fmap typeToTypeAST mtype)

typeToTypeAST :: A.Type -> L.TypeAST
typeToTypeAST (A.TName s ts) = foldl1 L.AppT $ (L.ConT $ idToName s):
                               map typeToTypeAST ts
typeToTypeAST (A.TVar s) = L.VarT $ idToName s
typeToTypeAST (A.TFun ts) = foldr1 arrowt $ map typeToTypeAST ts
    where
      arrowt a b = L.ConT(L.Name(Just["Prim"])"->") `L.AppT` a `L.AppT` b

expToAST (A.EVar id) = L.Named $ idToName id
expToAST (A.ECon id) = L.Named $ idToName id
expToAST (A.ELit lit) = L.LitAST $ aLitToLLit lit
expToAST (A.ETuple exps) = L.TupleAST $ map expToAST exps
expToAST (A.ELambda ps e) = L.LamAST (map patToPatAST ps) $ expToAST e
expToAST (A.EApp f x) = L.AppAST (expToAST f) (expToAST x)
expToAST (A.ECase exp clauses) = L.CaseAST (expToAST exp) $
                                 map (\(pat,exp) ->
                                          (patToPatAST pat,expToAST exp))
                                 clauses

patToPatAST :: A.Pattern -> L.PatAST
patToPatAST (A.PCon id) = L.ConPat $ idToName id
patToPatAST (A.PLit lit) = L.LitPat $ aLitToLLit lit
patToPatAST A.PWild = L.WildPat
patToPatAST (A.PTuple ps) = L.TuplePat $ map patToPatAST ps

aLitToLLit :: A.Literal -> L.Lit
aLitToLLit (A.LS s) = L.StringL s
aLitToLLit (A.LC c) = L.CharL c
aLitToLLit (A.LI i) = L.IntegerL i
aLitToLLit (A.LD d) = L.DoubleL d

--------------------------------------------------------------------------------
--And now in the other direction...
--------------------------------------------------------------------------------

tcModToModule :: L.TCModule -> A.Module A.Type
tcModToModule (L.TCModule n ns ns'asts'types) = 
    A.Mod (nameToId n) (map nameToId ns) (map toFunction ns'asts'types)
nameToId (Name _ s) = s
toFunction (n,ast,typ) = A.Fun (nameToId n) (typeASTToType typ) (astToExp ast)

typeASTToType (L.AppT )