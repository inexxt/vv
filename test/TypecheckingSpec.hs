module TypecheckingSpec where

import SpecHelper
import Typechecking
import Eval


-- some variable names
[z, s, m, n] = map (Var . (: [])) "zsmn"

app2 f x y = App (App f x) y
app3 f x y z = App (App (App f x) y) z

boolT :: Expr
boolT = Pi "Bool" (U 0) (Pi "b1" (Var "Bool") (Pi "b2" (Var "Bool") (Var "Bool")))

true = Lam "Bool" (U 0) (Lam "b1" (Var "Bool") (Lam "b2" (Var "Bool") (Var "b1")))
false = Lam "Bool" (U 0) (Lam "b1" (Var "Bool") (Lam "b2" (Var "Bool") (Var "b2")))

spec :: Spec
spec = do
    describe "nf" $ do
        context "with true a b" $
            it "should be a" $
                nf (app3 true (Var "Bool") (Var "a") (Var "b")) `shouldBe` Var "a"
        context "with false a b" $
            it "should be a" $
                nf (app3 false (Var "Bool") (Var "a") (Var "b")) `shouldBe` Var "b"
    describe "typecheck" $ do
        context "with true boolT" $
            it "should be Pi" $
                typecheck [("Bool", U 0)] (App true (Var "Bool")) `shouldBe` Right (Pi "b1'" (Var "Bool") (Pi "b2" (Var "Bool") (Var "Bool")))
    describe "typecheck" $ do
        context "with Pi (U 0) (U 1)" $
            it "should be U 2" $
                typecheck [] (Pi "a" (U 0) (U 1)) `shouldBe` Right (U 2)

main :: IO ()
main = hspec spec