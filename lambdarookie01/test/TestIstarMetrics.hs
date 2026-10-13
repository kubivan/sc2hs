module TestIstarMetrics (istarMetricTests) where

import Istar (ageScore, openPathScore, progressScore, safetyScore, towardsPathScore)
import SC2.Grid (gridFromLines, gridFromList)
import Test.Hspec
import VisionDecay (FogTile (..), FogTileState (..))

istarMetricTests :: Spec
istarMetricTests = describe "Istar metrics" $ do
  let openGrid =
        gridFromLines
          [ "#########"
          , "#   #   #"
          , "#       #"
          , "#       #"
          , "#########"
          ]
      pathGrid =
        gridFromLines
          [ "#######"
          , "#     #"
          , "#     #"
          , "#     #"
          , "#     #"
          , "#     #"
          , "#######"
          ]

  it "scores longer open rays higher" $ do
    let blockedRayScore = openPathScore openGrid (1, 1) (2, 1)
        openRayScore = openPathScore openGrid (1, 2) (2, 2)
    blockedRayScore `shouldBe` (4 / 14)
    openRayScore `shouldBe` (8 / 18)
    openRayScore `shouldSatisfy` (> blockedRayScore)

  it "scores movement toward the destination higher than movement away" $ do
    let towardScore = progressScore (2, 3) (3, 3) (5, 3)
        awayScore = progressScore (2, 3) (1, 3) (5, 3)
    towardScore `shouldSatisfy` (> awayScore)
    abs (towardScore - (0.5 + 1 / (2 * sqrt 2))) `shouldSatisfy` (< 0.000001)
    abs (awayScore - (0.5 - 1 / (2 * sqrt 2))) `shouldSatisfy` (< 0.000001)

  it "averages age scores for unknown, blocked, fresh, and old vision tiles" $ do
    let tiles =
          [ FogTile 9 FogTileUnknown
          , FogTile 0 FogTileBlocked
          , FogTile 0 FogTileClear
          , FogTile 500 FogTileClear
          ]
    ageScore tiles `shouldBe` 0.375
    ageScore [] `shouldBe` 0

  it "reduces safety as nearby threat increases" $ do
    let heatMap =
          gridFromList
            7
            7
            [ if (x, y) == (4, 3) then 20 else 0
            | y <- [0 .. 6 :: Int]
            , x <- [0 .. 6 :: Int]
            ]
    safetyScore heatMap (3, 3) `shouldBe` 0.5
    safetyScore (gridFromList 7 7 (replicate 49 0)) (3, 3) `shouldBe` 1

  it "scores a candidate by its traversable distance to the next waypoint" $ do
    towardsPathScore pathGrid (1, 3) (5, 3) `shouldBe` 0.6
