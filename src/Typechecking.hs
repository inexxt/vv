module Typechecking where

import Data.List
import Control.Monad
import Control.Monad.Except
import Data.Bifunctor
import Numeric.Natural

import Err
import Eval
import PrettyPrinter
import Debug.Trace

{-# ANN module "HLint: ignore Eta reduce" #-}

type TC = Either ErrMsg

variableLookupM :: Env -> Name -> Maybe Expr
variableLookupM [] n = Nothing
variableLookupM ((e, tt) : es) n =
  if e == n
    then return tt
    else variableLookupM es n

-- Splitting the function to get more informative error messages
variableLookup :: Env -> Name -> TC Expr
variableLookup env n = case variableLookupM env n of
    Nothing -> throwError $ "Variable '" ++ n ++ "' not found in env " ++ prettyPrintEnv env
    Just x -> return x

extend :: Env -> Name -> Expr -> Env
extend env name expr = (name, expr):env

throwExprErr :: Expr -> Expr -> Expr -> Expr -> Env -> TC ()
throwExprErr e tExp tGot tBased env = 
    throwError $ 
      "Function " 
      ++ prettyPrint e
      ++ " expects type " 
      ++ prettyPrint tExp ++ 
      " instead got " 
      ++ prettyPrint tGot
      ++ "(based on expression "
      ++ prettyPrint tBased
      ++ ")\n"
      ++ "in env "
      ++ prettyPrintEnv (reverse env)

throwUTypeErr :: Expr -> Expr -> Expr -> Expr -> Expr -> Env -> TC Expr
throwUTypeErr elem ttin tin ttout tout env =
  throwError $
    "Types should have universe types.\n"
      ++ "Instead, "
      ++ prettyPrint elem
      ++ " has input type "
      ++ prettyPrint ttin
      ++ " (based on expression "
      ++ prettyPrint tin
      ++ ")"
      ++ " and output type "
      ++ prettyPrint ttout
      ++ " (based on expression "
      ++ prettyPrint tout
      ++ ")\n"
      ++ "in env "
      ++ prettyPrintEnv (reverse env)

throwExpectedFunctionErr :: Expr -> Expr -> Env -> TC Expr
throwExpectedFunctionErr e tGot env =
  throwError $
    "Expected expression " ++ prettyPrint e ++ " to have a function type.\n"
      ++ "Instead, it has a type "
      ++ prettyPrint tGot
      ++ "\n in env "
      ++ prettyPrintEnv (reverse env)

typecheckNF :: Env -> Expr -> TC Expr
typecheckNF env (Var x) = variableLookup env x
typecheckNF env (U k) = return $ U (k + 1)
typecheckNF env (App e1 e2) = do -- 2
        t1 <- typecheckNF env (nf e1)
        case nf t1 of
          (Pi x tt ee) -> do
            t2 <- typecheckNF env e2
            unless (betaEq tt t2) $ throwExprErr e1 tt t2 e2 env
            return (nf (substitute x e2 ee))
          _ -> throwExpectedFunctionErr (nf e1) t1 env
typecheckNF env (Lam x tin e) = let env' = extend env x (nf tin) in do
    typecheckNF env tin
    tout <- typecheckNF env' e
    let rt = Pi x tin tout
    typecheckNF env rt
    return (nf rt)
typecheckNF env elem@(Pi x tin e) = let env' = extend env x (nf tin) in do
    ttin <- typecheckNF env tin
    ttout <- typecheckNF env' e
    case (nf ttin, nf ttout) of
      (U kin, U kout) -> return $ U (max kin kout)
      (ttin, ttout) -> throwUTypeErr (nf elem) ttin tin ttout (nf e) env

typecheck :: Env -> Expr -> TC Expr
typecheck env expr = typecheckNF (map (second nf) env) (nf expr)