module Day10Spec (spec) where

import Test.Hspec
import Day10

spec :: Spec
spec =
    describe "Day10 tests" $ do
    it "sameLine true" $
        sameLine (0,0) (3,1) (6,2) `shouldBe` True 
    it "sameLine true vertical" $
        sameLine (1,1) (2,1) (3,1) `shouldBe` True
    it "sameLine true horizontal" $
        sameLine (1,1) (1,2) (1,3) `shouldBe` True
    it "sameLine false" $
        sameLine (3,1) (3,2) (6,2) `shouldBe` False
    it "sameLine false 2" $
        sameLine (3,2) (6,4) (8,6) `shouldBe` False
    it "pointNotVisible1" $
        pointNotVisible (1,1) (2,2) (3,3) `shouldBe` True 
    it "pointNotVisible2" $
        pointNotVisible (2,2) (1,1) (3,3) `shouldBe` False  