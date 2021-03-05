module Lambda.UntypedSpec where

import SpecHelper
import Lambda.Untyped

spec :: Spec
spec = 
    describe "plus" $ do
        context "with one two" $
          it "should be three" $
            betaEq (app2 plus one two) three `shouldBe` True

main :: IO ()
main = hspec spec