module TestVisionDecay (visionDecayTests) where

import SC2.Grid (gridFromLines)
import SC2.Ids.UnitTypeId (UnitTypeId (ProtossProbe))
import SC2.Proto.Data (Alliance (Enemy))
import Test.Hspec
import TestFixtures (mockUnit)
import VisionDecay

visionDecayTests :: Spec
visionDecayTests = describe "VisionDecay" $ do
  let grid = gridFromLines ["#####", "#   #", "#####"]

  it "initializes walkable tiles as unknown and walls as blocked" $ do
    let vision = visionDecayEmpty grid
    visionDecayTile vision (1, 1) `shouldBe` FogTile 0 FogTileUnknown
    visionDecayTile vision (0, 1) `shouldBe` FogTile 0 FogTileBlocked

  it "ages observed enemies while unknown and blocked tiles remain unchanged" $ do
    let enemy = mockUnit 7 ProtossProbe Enemy (2, 1, 0)
        initial = visionDecayEmpty grid
        withEnemy = visionDecayAddEnemy initial enemy
        stepped = visionDecayStep withEnemy
    visionDecayTile withEnemy (2, 1) `shouldBe` FogTile 0 (FogTileUnit enemy)
    visionDecayTile stepped (2, 1) `shouldBe` FogTile 1 (FogTileUnit enemy)
    visionDecayTile stepped (1, 1) `shouldBe` FogTile 0 FogTileUnknown
    visionDecayTile stepped (0, 1) `shouldBe` FogTile 0 FogTileBlocked

  it "moves a tracked enemy and clears its previous tile" $ do
    let enemy = mockUnit 7 ProtossProbe Enemy (2, 1, 0)
        movedEnemy = mockUnit 7 ProtossProbe Enemy (3, 1, 0)
        moved =
          visionDecayAddEnemy
            (visionDecayAddEnemy (visionDecayEmpty grid) enemy)
            movedEnemy
    visionDecayTile moved (2, 1) `shouldBe` FogTile 0 FogTileUnknown
    visionDecayTile moved (3, 1) `shouldBe` FogTile 0 (FogTileUnit movedEnemy)
