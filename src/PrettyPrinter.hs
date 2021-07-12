module PrettyPrinter where

import Eval
import Data.List

hideArgs :: Expr -> Expr
hideArgs (Lam _ _ e) = hideArgs e
hideArgs e = e

removeApostroph :: String -> String
removeApostroph x = if last x == '\'' then removeApostroph (init x) else x

prettyPrint :: Expr -> String
prettyPrint (Var x) = removeApostroph x
prettyPrint (Pi n t e) =
  case name of
    "_" -> "(" ++ prettyPrint t ++ " -> " ++ prettyPrint e ++ ")"
    _ -> "(" ++ name ++ " : " ++ prettyPrint t ++ ") -> " ++ prettyPrint e
  where
    name = removeApostroph n
prettyPrint (Lam n t e) = "λ (" ++ removeApostroph n ++ " : " ++ prettyPrint t ++ ") -> " ++ prettyPrint e
prettyPrint (U k) = "U " ++ show k
prettyPrint (App e1 e2) = "(" ++ prettyPrint e1 ++ " " ++ prettyPrint e2 ++ ")"

prettyPrintEnv :: Env -> String
prettyPrintEnv env =
  "[\n\t" ++ intercalate ",\n\t" (fmap (\(n, s) -> removeApostroph n ++ " :: " ++ prettyPrint s) env) ++ "\n]"
