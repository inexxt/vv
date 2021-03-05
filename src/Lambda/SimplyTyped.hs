module Lambda.SimplyTyped
  ( Name,
    Expr,
    Type,
    nf,
    freeVars,
    alphaEq,
    betaEq,
    zero,
    one,
    two,
    three,
    plus,
    app2,
    typecheck,
    initialEnv
  )
where

import Data.List

{-# ANN module "HLint: ignore Eta reduce" #-}

type Name = String
data Type = Base | Arrow Type Type deriving (Eq, Read, Show)

data Expr
  = Var Name
  | App Expr Expr
  | Lam Name Type Expr
  deriving (Eq, Read, Show)

type Env = [(Name, Type)]

whnf :: Expr -> Expr
whnf ee = spine ee []
  where
    spine (App f a) as = spine f (a : as)
    spine (Lam s tt e) (a : as) = spine (subst s a e) as
    spine f as = foldl App f as

freeVars :: Expr -> [Name]
freeVars (Var s) = [s]
freeVars (App a b) = freeVars a `union` freeVars b
freeVars (Lam s tt e) = freeVars e \\ [s]

subst :: Name -> Expr -> Expr -> Expr
subst y t e = sub e
  where
    sub e@(Var v) = if v == y then t else e
    sub (App e1 e2) = App (sub e1) (sub e2)
    sub (Lam x tt e)
      | y == x = Lam x tt e
      | otherwise =
        if x `elem` fvt
          then
            let x' = cloneSym e x -- choose a new name for x
                e' = subst x (Var x') e
             in Lam x' tt (sub e')
          else Lam x tt (sub e)
    fvt = freeVars t
    cloneSym e v = loop v
      where
        loop v' = if v' `elem` vars then loop (v ++ "'") else v'
        vars = fvt ++ freeVars e

alphaEq :: Expr -> Expr -> Bool
alphaEq (Var x) (Var y) = x == y
alphaEq (App e1 e2) (App f1 f2) = alphaEq e1 f1 && alphaEq e2 f2
alphaEq (Lam x1 tt1 e1) (Lam x2 tt2 e2) = alphaEq e1 (subst x2 (Var x1) e2) && tt1 == tt2
alphaEq _ _ = False

nf :: Expr -> Expr
nf ee = spine ee []
  where
    spine (App e1 e2) ts = spine e1 (e2 : ts)
    spine (Lam x tt e) [] = Lam x tt (nf e)
    spine (Lam x tt e) (t : ts) = spine (subst x t e) ts
    spine e ts = app e ts
    app e ts = foldl App e (map nf ts)

betaEq :: Expr -> Expr -> Bool
betaEq e1 e2 = alphaEq (nf e1) (nf e2)

type EM = Either String

variableLookup :: Env -> Name -> EM Type
variableLookup [] n = Left ("Variable " ++ n ++ " not found in Env")
variableLookup ((e, tt):es) n =
    if e == n then Right tt
    else variableLookup es n

typecheck :: Env -> Expr -> EM Type
typecheck env (Var x) = variableLookup env x
typecheck env (App e1 e2) = 
        case (typecheck env e1, typecheck env e2) of
        (Left err, _) -> Left err
        (_, Left err) -> Left err
        (Right Base, _) -> Left ("Expression " ++ show e1 ++ " is not an application")
        (Right (Arrow tin tout), Right t2) -> 
            if tin == t2 then Right tout
            else Left ("Expression " ++ show e1 ++ "expects type " ++ show tin ++ " instead got " ++ show t2)

typecheck env (Lam x tin e) = 
    case typecheck ((x,tin):env) e of
    (Left err) -> Left err
    Right tout -> Right (Arrow tin tout)

[z, s, m, n] = map (Var . (: [])) "zsmn"

app2 f x y = App (App f x) y

zero = Lam "s" (Arrow Base Base) $ Lam "z" Base z

one = Lam "s" (Arrow Base Base) $ Lam "z" Base $ App s z

two = Lam "s" (Arrow Base Base) $ Lam "z" Base $ App s $ App s z

three = Lam "s" (Arrow Base Base) $ Lam "z" Base $ App s $ App s $ App s z

plus = Lam "m" (Arrow (Arrow Base Base) (Arrow Base Base)) $ 
       Lam "n" (Arrow (Arrow Base Base) (Arrow Base Base)) $ 
        Lam "s" (Arrow Base Base) $ Lam "z" Base $ app2 m s (app2 n s z)

initialEnv :: Env
initialEnv = []