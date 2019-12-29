module Day12Spec (spec) where

import Test.Hspec
import Day12

p0 = Pt (-1) 0 2
p1 = Pt 2 (-10) (-7)
p2 = Pt 4 (-8) 8
p3 = Pt 3 5 (-1)
v0 = Vl 0 0 0

spec :: Spec
spec =
    describe "Day12 tests" $ do
    it "apply gravities p0" $
        applyGravities (Si p0 (Vl 0 0 0)) [p1,p2,p3] `shouldBe` Si p0 (Vl 3 (-1) (-1))
    it "apply gravities p1" $
        applyGravities (Si p1 (Vl 0 0 0)) [p0,p2,p3] `shouldBe` Si p1 (Vl 1 3 3)
    it "step" $
        step [Si p0 v0, Si p1 v0, Si p2 v0, Si p3 v0] `shouldBe`
            [Si (Pt 2 (-1) 1) (Vl 3 (-1) (-1)),
            Si (Pt 3 (-7) (-4)) (Vl 1 3 3),
            Si (Pt 1 (-7) 5) (Vl (-3) 1 (-3)),
            Si (Pt 2 2 0) (Vl (-1) (-3) 1)]
    it "solve1" $
        solve1 [Si p0 v0, Si p1 v0, Si p2 v0, Si p3 v0] 10 `shouldBe` 179
