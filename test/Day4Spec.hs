module Day4Spec (spec) where

import Test.Hspec
import qualified Data.Set as Set
import Day4

spec :: Spec
spec =
    describe "Day4 tests" $ do
    it "toDigits 1" $
        toDigits 1 `shouldBe` [1]
    it "toDigits 2" $
        toDigits 123456 `shouldBe` [1,2,3,4,5,6]
    it "pairs construction" $
        pairwise [4,7,11,0] `shouldBe` [(4,7),(7,11),(11,0)]
    it "same digits detected" $    
        sameDigits 122345 `shouldBe` True
    it "no same digits detected" $
        sameDigits 987654 `shouldBe` False
    it "never decrease true 1" $
        neverDecrease 111123 `shouldBe` True 
    it "never decrease true 2" $
        neverDecrease 135679 `shouldBe` True 
    it "never decrease false 1" $
        neverDecrease 223450 `shouldBe` False
    it "never decrease false 2" $
        neverDecrease 293458 `shouldBe` False
    it "valid pass 1" $
        validPass 111111 `shouldBe` True 
    it "valid pass 2" $
        validPass 223450 `shouldBe` False  
    it "valid pass 3" $
        validPass 123789 `shouldBe` False 
    it "valid pass 3" $
        validPass 111123 `shouldBe` True  