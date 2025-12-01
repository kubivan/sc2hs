{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLabels #-}

module TestUnits (unitsUnitTests) where

import Lens.Micro ((&), (.~), (^.))
import SC2.Proto.Data (Alliance (Neutral, Self), Point)
import Proto.S2clientprotocol.Common_Fields qualified as C
import Proto.S2clientprotocol.Raw_Fields qualified as R
import SC2.Ids.UnitTypeId
import Test.Hspec
import Units

import Data.ProtoLens (defMessage)

unitsUnitTests :: Spec
unitsUnitTests = describe "Units" $ do
    it "recognises building types" $ do
        isBuildingType ProtossNexus `shouldBe` True
        isBuildingType ProtossStalker `shouldBe` False

    it "detects buildings from units" $ do
        isBuilding nexusUnit `shouldBe` True
        isBuilding stalkerUnit `shouldBe` False

    it "detects minerals and geysers" $ do
        isMineral mineralField `shouldBe` True
        isMineral geyserUnit `shouldBe` False
        isGeyser geyserUnit `shouldBe` True
        isGeyser mineralField `shouldBe` False

    it "computes bounding boxes" $ do
        unitsBoundingBox [nexusUnit, stalkerUnit]
            `shouldBe` ((10, 20), (30, 40))

    it "produces velocity vectors from facing" $ do
        let vec0 = unitVelocityVec (stalkerUnit & R.facing .~ 0)
            vec90 = unitVelocityVec (stalkerUnit & R.facing .~ (pi / 2))
        (vec0 ^. C.x) `shouldApprox` 4.13
        (vec0 ^. C.y) `shouldApprox` 0
        (vec90 ^. C.x) `shouldApprox` 0
        (vec90 ^. C.y) `shouldApprox` 4.13

shouldApprox :: Float -> Float -> Expectation
shouldApprox actual expected = abs (actual - expected) `shouldSatisfy` (< 1e-3)

nexusUnit :: Unit
nexusUnit =
    defMessage
        & R.tag .~ 1
        & R.unitType .~ fromEnum' ProtossNexus
        & R.alliance .~ Self
        & R.pos .~ pointAt 10 20

stalkerUnit :: Unit
stalkerUnit =
    defMessage
        & R.tag .~ 2
        & R.unitType .~ fromEnum' ProtossStalker
        & R.alliance .~ Self
        & R.pos .~ pointAt 30 40

mineralField :: Unit
mineralField =
    defMessage
        & R.tag .~ 3
        & R.unitType .~ fromEnum' NeutralMineralField
        & R.alliance .~ Neutral
        & R.mineralContents .~ 900
        & R.pos .~ pointAt 5 5

geyserUnit :: Unit
geyserUnit =
    defMessage
        & R.tag .~ 4
        & R.unitType .~ fromEnum' NeutralVespeneGeyser
        & R.alliance .~ Neutral
        & R.vespeneContents .~ 500
        & R.pos .~ pointAt 6 6

pointAt :: Float -> Float -> Point
pointAt x y = defMessage & C.x .~ x & C.y .~ y & C.z .~ 0
