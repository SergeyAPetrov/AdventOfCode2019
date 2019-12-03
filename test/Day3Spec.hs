module Day3Spec (spec) where

import Test.Hspec
import Day3

spec :: Spec
spec =
    describe "Day3 tests" $ do
    it "parse single entry path" $
        parse "U1" `shouldBe` [(U,1)]
    it "parse multi entry path" $
        parse "U1,R55" `shouldBe` [(U,1), (R,55)]
    it "wire step from start point" $
        wireStep (0, 0) (R, 3) `shouldBe` [(1,0), (2,0), (3,0)]
    it "wire next step" $
        wireStep (3, 0) (U, 4) `shouldBe` [(3,1), (3,2), (3,3), (3,4)]
    it "wire path" $
        wire [(R, 3), (U, 4)] `shouldBe` [(0,0),(1,0), (2,0), (3,0), (3,1), (3,2), (3,3), (3,4)]
    it "wire path left and down" $
        wire [(D, 2), (L, 3)] `shouldBe` [(0,0),(0,-1), (0,-2), (-1,-2), (-2,-2), (-3,-2)]
    it "intersection found" $
        wireIntersection "R8,U5,L5,D3" "U7,R6,D4,L4" `shouldBe` [(0,0),(6,5), (3,3)]
    it "solution test 1" $
        solve1 "R8,U5,L5,D3" "U7,R6,D4,L4" `shouldBe` 6
    it "solution test 2" $
        solve1 "R75,D30,R83,U83,L12,D49,R71,U7,L72" "U62,R66,U55,R34,D71,R55,D58,R83" `shouldBe` 159
    it "solution test 3" $
        solve1 "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51" "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7" `shouldBe` 135

        -- it "getFuel handles rounding integers down" $
    --     getFuel 14 `shouldBe` (2 :: Integer)
        