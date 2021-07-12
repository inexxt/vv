module Desugar where

import Parser.AbsVVSyntax
import Typechecking
import Eval
import Control.Monad.Except
import PrettyPrinter
import Debug.Trace

type Type = Expr

toName :: Ident -> Name
toName (Ident s) = s

definitionName :: Definition -> Name
definitionName (DefTE (DefType name' tt) (Eq name [] ee)) = toName name
definitionName (DefT (DefType name tt)) = toName  name
definitionName (DefTE (DefType name' tt) (Eq n (n2 : ns) ee)) = toName name'

desugarPatternMatch :: Name -> [TypeJudgement] -> Expr -> TC Expr
desugarPatternMatch fname []  expr = return expr
desugarPatternMatch fname ((DefType n dt):tts) expr = do
    rec <- desugarPatternMatch fname tts expr
    (_, ddt) <- desugarExpr dt
    return $ Lam (toName n) ddt rec

extractTypesSeq :: Expr -> [Expr]
extractTypesSeq (Pi name dt de) = dt : extractTypesSeq de
extractTypesSeq _ = []

desugarExpr :: ExprE -> TC (Env, Expr)
desugarExpr (Let [] ee) = do
    (_, de) <- desugarExpr ee
    return ([], de)
desugarExpr (Let ((DefTE (DefType name dt) (Eq name' ns de)):ss) ee) = do
    unless (name == name') $ throwError $ "Different identifiers in let expression, expected " ++ show name ++ " == " ++ show name'
    (_, ddt) <- desugarExpr dt
    (env, dee) <- desugarExpr $ Let ss ee
    (_, dde) <- desugarExpr de
    full_dde <- desugarPatternMatch (toName name) ns dde
    -- dde_type <- typecheck env full_dde
    -- unless (betaEq dde_type ddt) $ throwError $ "Expected types " ++ prettyPrint dde_type ++ " and " ++ prettyPrint ddt ++ " to be equal"
    return ((toName name, ddt) : env, App (Lam (toName name) ddt dee) full_dde)
-- desugarExpr (Let ((DefE (Eq n de)) : ss) ee) = do
--     ddt <- desugarExpr dt
--     dee <- desugarExpr $ Let ss ee
--     dde <- desugarExpr de
--     return $ App (Lam (toName n) tt (desugarExpr $ Let ss ee)) de
desugarExpr (Let ((DefT (DefType name dt)) : ss) ee) = do
    (_, dtt) <- desugarExpr dt
    (env, dret) <- desugarExpr $ Let ss ee
    return ((toName name, dtt) : env, Lam (toName name) dtt dret)
desugarExpr (LamE (DefType n dt) ee) = do
    (_, ddt) <- desugarExpr dt
    (_, dee) <- desugarExpr ee
    return ([], Lam (toName n) ddt dee)
desugarExpr (VarE n) = return ([], Var (toName n))
desugarExpr (AppE e1 e2) = do
    (_, de1) <- desugarExpr e1
    (_, de2) <- desugarExpr e2
    return ([], App de1 de2)
desugarExpr (UE k) = return ([], U (fromInteger k))
desugarExpr (PiE (DefType n dt) ee) = do
    (_, ddt) <- desugarExpr dt
    (_, dee) <- desugarExpr ee
    return ([], Pi (toName n) ddt dee)
desugarExpr (Arr dt ee) = do
    (_, ddt) <- desugarExpr dt
    (_, dee) <- desugarExpr ee 
    return ([], arrow ddt dee)

desugarTopLevel :: [Definition] -> TC (Env, Expr)
desugarTopLevel [] = 
    throwError "Program should not be empty"
desugarTopLevel l@(DefTE (DefType name' tt) (Eq name [] ee):ss) = do
    unless (toName name == "main")
        $ throwError ("Last definition should define 'main', instead defines '" ++ toName name' ++ "'")
    unless (toName name == "main")
        $ throwError ("Last definition should define 'main', instead defines '" ++ toName name ++ "'")
    desugarExpr (Let (reverse ss) ee)
-- desugarTopLevel ss main@(DefE (Eq n ee)) = do
--     unless (n == "main") $ throwError "Last definition should define main"
    -- return (desugarExpr (Let ss main))
desugarTopLevel (main@(DefT (DefType name tt)):ss) = 
    throwError $ "Last definition should define 'main', now it only specifies type " ++ toName name
desugarTopLevel (DefTE (DefType name' tt) (Eq n (n2:ns) ee) : ss) = do
    unless (toName name' == "main") $
      throwError ("Last definition should define 'main', instead defines '" ++ toName name' ++ "'")
    throwError $ "'main' should not take any parameters - currently takes " ++ show (n2:ns)