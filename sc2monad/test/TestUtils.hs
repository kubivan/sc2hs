module TestUtils (utilsUnitTests) where

import Test.Hspec
import Utils (triPartition)

utilsUnitTests :: Spec
utilsUnitTests = describe "Utils" $ do
    it "triPartition splits elements" $ do
        let (lt, eq, gt) = triPartition (compare 3) ([3, 1, 2, 3, 2, 4] :: [Int])
        lt `shouldBe` [4]
        eq `shouldBe` [3, 3]
        gt `shouldBe` [1, 2, 2]
