{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLabels #-}

module TestObservation (observationUnitTests) where

import Data.ProtoLens (defMessage)
import Lens.Micro ((&), (.~), (^.))
import Observation
import Proto.S2clientprotocol.Common_Fields qualified as C
import Proto.S2clientprotocol.Raw_Fields qualified as R
import Proto.S2clientprotocol.Sc2api (ResponseGameInfo)
import Proto.S2clientprotocol.Sc2api_Fields qualified as S
import SC2.Grid (Grid, gridFromLines, gridPixel)
import SC2.Grid.TilePos (TilePos, tilePos)
import SC2.Ids.AbilityId (AbilityId (HARVESTGATHERPROBE))
import SC2.Ids.UnitTypeId
import SC2.Proto.Data (Alliance (Self), Point2D)
import Test.Hspec
import Units qualified

observationUnitTests :: Spec
observationUnitTests = describe "Observation" $ do
    it "addOrder adds an order to the matching unit" $ do
        let updated = addOrder 1 HARVESTGATHERPROBE baseObservation
        case updated ^. S.rawData . R.units of
            [u] -> do
                let abilityIds = map (^. R.abilityId) (u ^. R.orders)
                abilityIds `shouldBe` [fromIntegral (fromEnum HARVESTGATHERPROBE)]
            other ->
                expectationFailure $ "unexpected units " ++ show (length other)

    it "addUnit appends a new unit" $ do
        let obs = addUnit ProtossProbe emptyObservation
        case obs ^. S.rawData . R.units of
            [u] -> (Units.toEnum' (u ^. R.unitType) :: UnitTypeId) `shouldBe` ProtossProbe
            other -> expectationFailure $ "unexpected units " ++ show (length other)

    it "enemyBaseLocation skips the player start" $ do
        let enemy = enemyBaseLocation gameInfoWithStarts baseObservation
        enemy `shouldBe` point2D 40 40

    it "obsResources extracts minerals and gas" $ do
        let res = obsResources resourcesObservation
        res `shouldBe` Cost 400 125

    it "gridUpdate paints building footprints" $ do
        let updatedGrid = gridUpdate baseObservation emptyGrid
            tile :: TilePos
            tile = tilePos (baseUnit ^. R.pos)
        gridPixel updatedGrid tile `shouldBe` 'c'

emptyObservation :: Observation
emptyObservation = defMessage

baseUnit :: Units.Unit
baseUnit =
    defMessage
        & R.tag .~ 1
        & R.unitType .~ Units.fromEnum' ProtossNexus
        & R.alliance .~ Self
        & R.pos .~ (defMessage & C.x .~ 12 & C.y .~ 18 & C.z .~ 0)
        & R.buildProgress .~ 1

baseObservation :: Observation
baseObservation =
    defMessage
        & S.rawData .~ (defMessage & R.units .~ [baseUnit])

resourcesObservation :: Observation
resourcesObservation =
    defMessage
        & #playerCommon . #minerals .~ 400
        & #playerCommon . #vespene .~ 125

gameInfoWithStarts :: ResponseGameInfo
gameInfoWithStarts =
    defMessage
        & #startRaw .~ (defMessage & #startLocations .~ [point2D 12 18, point2D 40 40])

emptyGrid :: Grid
emptyGrid = gridFromLines (replicate 128 (replicate 128 ' '))

point2D :: Float -> Float -> Point2D
point2D x y = defMessage & C.x .~ x & C.y .~ y