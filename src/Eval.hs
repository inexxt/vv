module Eval (module Eval) where

import Control.Monad
import Control.Monad.Except
import Data.Bifunctor
import Data.List
import Numeric.Natural

{-# ANN module "HLint: ignore Eta reduce" #-}

type Name = String

data Expr
  = Var Name
  | App Expr Expr
  | Lam Name Expr Expr
  | Pi Name Expr Expr
  | U Natural
  deriving (Eq, Read, Show)

type Env = [(Name, Expr)]

freshEnv :: Env
freshEnv = []

freeVariables :: Expr -> [Name]
freeVariables (Var s) = [s]
freeVariables (App a b) = freeVariables a `union` freeVariables b
freeVariables (Lam s tt e) = freeVariables e `union` (freeVariables tt \\ [s])
freeVariables (Pi s tt e) = freeVariables tt `union` (freeVariables e \\ [s])
freeVariables (U _) = []

rename :: Name -> [Name] -> Name
rename name taken = loop name
  where
    loop name = if name `elem` taken then loop (name ++ "'") else name

freshName :: [Name] -> Name
freshName taken = rename "_" taken

substitute :: Name -> Expr -> Expr -> Expr
substitute y t e = sub e
  -- induction over expression
  -- keeping y and t constant
  where
    sub e@(Var v) = if v == y then t else e
    sub (U k) = U k
    sub (App e1 e2) = App (sub e1) (sub e2)
    sub (Lam x tt e) = subInType Lam x tt e
    sub (Pi x tt e) = subInType Pi x tt e
    subInType c x tt e =
      let stt = substitute y t tt
      in  if y == x then c x stt e
          else if x `elem` freeVars then 
                  -- the only non-trivial case is when
                  -- x is free in the expression
                  let x' = rename x freeVars
                      e' = substitute x (Var x') e
                  in  c x' stt (sub e')
               else c x stt (sub e)
    freeVars = freeVariables t ++ freeVariables e

alphaEq :: Expr -> Expr -> Bool
alphaEq (Var x) (Var y) = x == y
alphaEq (U x) (U y) = x == y
alphaEq (App e1 e2) (App f1 f2) = alphaEq e1 f1 && alphaEq e2 f2
alphaEq (Lam x1 tt1 e1) (Lam x2 tt2 e2) = alphaEq e1 (substitute x2 (Var x1) e2) && tt1 == tt2
alphaEq (Pi x1 tt1 e1) (Pi x2 tt2 e2) = alphaEq e1 (substitute x2 (Var x1) e2) && tt1 == tt2
alphaEq _ _ = False

nf :: Expr -> Expr
nf ee = reduction ee []
  where
    reduction (App e1 e2) ts = reduction e1 (e2 : ts)
    reduction (Lam x tt e) [] = Lam x tt (nf e)
    reduction (Lam x tt e) (t : ts) = reduction (substitute x t e) ts
    reduction e ts = app e ts
    app e ts = foldl App e (map nf ts)

betaEq :: Expr -> Expr -> Bool
betaEq e1 e2 = alphaEq (nf e1) (nf e2)

-- Non-dependent function type - helper
arrow :: Expr -> Expr -> Expr
arrow t1 t2 = Pi (freshName (freeVariables t2 ++ freeVariables t1)) t1 t2
