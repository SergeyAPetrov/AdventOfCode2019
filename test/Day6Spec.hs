module Day6Spec (spec) where

import Test.Hspec
import Day6
import Data.Tree

treeStr = ["COM)B", "B)C", "C)D","D)E" ,"E)F",  "B)G","G)H","D)I","E)J","J)K","K)L"]

spec :: Spec
spec =
    describe "Day6 tests" $ do
    it "root childs" $
        getNodes treeStr "COM" `shouldBe` ["B"]
    it "level 1 childs" $
        getNodes treeStr "B" `shouldBe` ["C", "G"]
    it "leaf childs" $
        getNodes treeStr "L" `shouldBe` []
    it "build tree" $
        buildTree ["A)B", "B)C", "B)D"] "A" `shouldBe` Node "A" [Node "B" [Node "C" [], Node "D" []]]
    it "find root path" $ 
        findRootPath (buildTree treeStr "COM") "K" `shouldBe` ["COM", "B","C", "D", "E", "J", "K"]