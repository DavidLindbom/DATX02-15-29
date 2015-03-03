
module TC where
import Control.Monad.State
--import Control.Monad.Trans.Either

data Exp = LitE Lit
         | LamE [Pat] Exp
         | NameE Name
         | CaseE [(Pat,Exp)]
         | TupleE [Exp]
         | AppE Exp Exp
           deriving (Eq,Show)
--polymorphic type.
data Type = TyVarT TyVar
          | ConT Name
          | AppT Type Type
            deriving (Eq,Show)
data Pat = WildP
         | ConP Name [Pat]
         | VarP Name
           deriving (Eq,Show)
type Name = String
type TyVar = String
data Lit = StringL String
         | NumDouble Double
         | NumInteger Integer
         | AtomL String
           deriving (Eq,Show)
type ErrorMessage = String
type Constraint = Either ErrorMessage [(TyVar,Type)] 
--error messages for failure        

typeCheck :: [(Name,Maybe Type,Exp)]->Either ErrorMessage [(Name,Type)]
typeCheck = error "TBD"

type TCMonad = (StateT (Int,Constraint) (Either ErrorMessage) [(Name,Type)])

--f,g, e: 
--f = (... f :: t ... g :: a ... f :: t) :: t
--get types for all, check if g of f is instance
--of G

unify :: Type -> Type -> Constraint
unify (TyVarT tvn) t | noncyclical tvn t = Right [(tvn,t)]
unify t (TyVarT tvn) | noncyclical tvn t = Right [(tvn,t)]
unify (AppT a b) (AppT c d) = do c1 <- unify a c
                                 c2 <- unify b d
                                 return $ merge c1 c2
unify (ConT n1) (ConT n2) | n1 == n2 = Right []
unify a b = Left $ "Can't unify: " ++ show a ++ ";" ++ show b
 
isMoreGeneralThan :: Type -> Type -> Bool
isMoreGeneralThan t1 t2 = case isMoreGeneralThan' t1 t2 of
                            Right _ -> True
                            _ -> False
isMoreGeneralThan' :: Type -> Type -> Constraint
isMoreGeneralThan' = undefined

--noncyclical checks whether the given type variable
--occurs in the given type.
noncyclical :: TyVar -> Type -> Bool
noncyclical tvn (TyVarT tvn') = tvn == tvn'
noncyclical tvn (ConT _) = False
noncyclical tvn (AppT a b) = noncyclical tvn a && noncyclical tvn b
                          
merge :: [(TyVar,Type)] -> [(TyVar,Type)] -> [(TyVar,Type)]
merge = undefined

--A >= A = True;
--a >= A = True;
--f x >= g y = (f > g) (x > y);
--t1 > t2 iff there exists
--a substitution subst of type variables,
--(that don't form a cycle) 
--to types s.t. subst t1 == t2
