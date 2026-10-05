module TestVisitedDecay (visitedDecayTests) where

import SC2.Grid (gridFromLines)
import Test.Hspec
import VisitedDecay

visitedDecayTests :: Spec
visitedDecayTests = describe "VisitedDecay" $ do
  it "distinguishes unvisited tiles from recently visited tiles" $ do
    let visits = visitedDecayEmpty $ gridFromLines ["  "]
    visitedDecayAge visits (0, 0) `shouldBe` Nothing
    visitedDecayScore visits (0, 0) `shouldBe` 0

  it "reduces the visit penalty as time passes" $ do
    let visits = visitedDecayVisit (0, 0) $ visitedDecayEmpty $ gridFromLines ["  "]
        oneStepLater = visitedDecayStep visits
    visitedDecayAge visits (0, 0) `shouldBe` Just 0
    visitedDecayScore visits (0, 0) `shouldBe` 1
    visitedDecayAge oneStepLater (0, 0) `shouldBe` Just 1
    visitedDecayScore oneStepLater (0, 0) `shouldBe` 0.5

  it "resets the age when a tile is visited again" $ do
    let visits =
          visitedDecayVisit (0, 0) $
            visitedDecayStep $
              visitedDecayVisit (0, 0) $
                visitedDecayEmpty (gridFromLines ["  "])
    visitedDecayAge visits (0, 0) `shouldBe` Just 0
