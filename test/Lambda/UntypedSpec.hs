module Lambda.UntypedSpec where

import SpecHelper

spec :: Spec
spec = 
    describe "sumGs" $ do
        context "with two three" $
          it "should be five" $
            betaEq (app2 plus one two) three `shouldBe` True

main :: IO ()
main = hspec spec