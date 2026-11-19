module TestVisitedDecay (visitedDecayTests) where

import Data.List (foldl')
import SC2.Grid (gridFromLines)
import Test.Hspec
import VisitedDecay

visitedDecayTests :: Spec
visitedDecayTests = describe "VisitedDecay" $ do
  it "uses the current frame for unvisited tiles" $ do
    let visits = visitedDecayEmpty $ gridFromLines ["  "]
    visitedDecayAge 100 visits (0, 0) `shouldBe` 0

  it "computes age from the recorded visit frame" $ do
    let visits = visitedDecayVisit 100 (0, 0) $ visitedDecayEmpty $ gridFromLines ["  "]
    visitedDecayAge 100 visits (0, 0) `shouldBe` 0
    visitedDecayAge 150 visits (0, 0) `shouldBe` 50

  it "resets the visited tile while preserving other visit times" $ do
    let visits = visitedDecayVisit 200 (1, 0) $
          visitedDecayVisit 100 (0, 0) $ visitedDecayEmpty $ gridFromLines ["  "]
        revisited = visitedDecayVisit 300 (0, 0) visits
    visitedDecayAge 350 revisited (0, 0) `shouldBe` 50
    visitedDecayAge 350 revisited (1, 0) `shouldBe` 150

  it "supports repeated updates without reading the history between visits" $ do
    let initial = visitedDecayEmpty $ gridFromLines ["  "]
        visits = foldl' (\history frame -> visitedDecayVisit frame (0, 0) history) initial [1 .. 10000]
    visitedDecayAge 10001 visits (0, 0) `shouldBe` 1
