module Day1Spec (spec) where

import Test.Hspec
import Day1

spec :: Spec
spec =
    describe "Day1 tests" $ do
    it "given mass, fuel required is mass / 3 - 2" $
        getFuel 12 `shouldBe` (2 :: Integer)
    it "getFuel handles rounding integers down" $
        getFuel 14 `shouldBe` (2 :: Integer)
    it "getFuel extra test 1" $
        getFuel 1969 `shouldBe` (654 :: Integer)
    it "getFuel extra test 2" $
        getFuel 100756 `shouldBe` (33583 :: Integer)
    it "given that the fuel mass is small, no additional fuel is required for the total" $
        getFuelTotal 14 `shouldBe` (2 :: Integer)
    it "getFuelTotal takes fuel mass into account" $
        getFuelTotal 1969 `shouldBe` (966 :: Integer)
    it "getFuelTotal extra test 1" $
        getFuelTotal 100756 `shouldBe` (50346 :: Integer)