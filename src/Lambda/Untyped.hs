module Lambda.Untyped (
    Name,
    Expr,
    nf,
    freeVars,
    alphaEq,
    betaEq,
    zero,
    one,
    two,
    three,
    plus,
    app2
) where

import Data.List 

{-# ANN module "HLint: ignore Eta reduce" #-}

type Name = String

data Expr = Var Name
          | App Expr Expr
          | Lam Name Expr
          deriving (Eq, Read, Show)

whnf :: Expr -> Expr
whnf ee = spine ee []
    where spine (App f a) as = spine f (a:as)
          spine (Lam s e) (a:as) = spine (subst s a e) as
          spine f as = foldl App f as

freeVars :: Expr -> [Name]
freeVars (Var s) = [s]
freeVars (App a b) = freeVars a `union` freeVars b
freeVars (Lam s e) = freeVars e \\ [s]

subst :: Name -> Expr -> Expr -> Expr
subst y t e = sub e
    where sub e@(Var v) = if v == y then t else e
          sub (App e1 e2) = App (sub e1) (sub e2)
          sub (Lam x e) 
                | y == x = Lam x e
                | otherwise =
                    if x `elem` fvt then
                        let x' = cloneSym e x -- choose a new name for x
                            e' = subst x (Var x') e
                        in  Lam x' (sub e')
                    else
                        Lam x (sub e)
          fvt = freeVars t
          cloneSym e v = loop v
              where loop v' = if v' `elem` vars then loop (v ++ "'") else v'
                    vars = fvt ++ freeVars e

alphaEq :: Expr -> Expr -> Bool
alphaEq (Var x) (Var y) = x == y
alphaEq (App e1 e2) (App f1 f2) = alphaEq e1 f1 && alphaEq e2 f2
alphaEq (Lam x1 e1) (Lam x2 e2) = alphaEq e1 (subst x2 (Var x1) e2)
alphaEq _ _ = False

nf :: Expr -> Expr
nf ee = spine ee []
    where spine (App e1 e2) ts = spine e1 (e2:ts)
          spine (Lam x e) [] = Lam x (nf e)
          spine (Lam x e) (t:ts) = spine (subst x t e) ts
          spine e ts = app e ts
          app e ts = foldl App e (map nf ts)

betaEq :: Expr -> Expr -> Bool
betaEq e1 e2 = alphaEq (nf e1) (nf e2)

[z, s, m, n] = map (Var . (: [])) "zsmn"

app2 f x y = App (App f x) y

zero = Lam "s" $ Lam "z" z

one = Lam "s" $ Lam "z" $ App s z

two = Lam "s" $ Lam "z" $ App s $ App s z

three = Lam "s" $ Lam "z" $ App s $ App s $ App s z

plus = Lam "m" $ Lam "n" $ Lam "s" $ Lam "z" $ app2 m s (app2 n s z)
