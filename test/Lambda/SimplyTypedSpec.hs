module Lambda.SimplyTypedSpec where

import SpecHelper
import Lambda.SimplyTyped
    ( app2, initialEnv, one, plus, two, typecheck, Type , base , arrow )

spec :: Spec
spec = 
    describe "typecheck" $ do
        context "with plust two three" $
          it "should be Arrow (Arrow Base Base) (Arrow Base Base)" $
            typecheck initialEnv (app2 plus one two) `shouldBe` Right (arrow (arrow base base) (arrow base base))

main :: IO ()
main = hspec spec