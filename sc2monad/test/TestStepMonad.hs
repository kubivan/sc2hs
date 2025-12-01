{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLabels #-}

module TestStepMonad (stepMonadUnitTests) where

import Actions (Action (..), DebugCommand (..))
import Data.HashMap.Strict qualified as HashMap
import Data.ProtoLens (defMessage)
import Data.Text qualified as T
import Lens.Micro ((&), (.~), (^.), (^..))
import Observation (Observation)
import Proto.S2clientprotocol.Common_Fields qualified as C
import Proto.S2clientprotocol.Raw_Fields qualified as R
import Proto.S2clientprotocol.Sc2api_Fields qualified as S
import SC2.Grid (Grid, gridFromLines)
import SC2.Ids.AbilityId (AbilityId (HARVESTGATHERPROBE))
import SC2.Ids.UnitTypeId (UnitTypeId (ProtossProbe))
import StepMonad
import Test.Hspec
import Units (Unit)
import Units qualified
import SC2.Proto.Data (Alliance (Self), Point)

-- Small dynamic state for exercising StepMonad helpers.
data DummyState = DummyState
    { dummyObs :: Observation
    , dummyGrid :: Grid
    }

instance AgentDynamicState DummyState where
    getObs = dummyObs
    getGrid = dummyGrid
    setObs obs st = st {dummyObs = obs}
    setGrid grid st = st {dummyGrid = grid}
    dsUpdate obs grid st = st {dummyObs = obs, dummyGrid = grid}

stepMonadUnitTests :: Spec
stepMonadUnitTests =
    describe "StepMonad" $ do
        it "command updates step plan and observation" $ do
            let action = SelfCommand HARVESTGATHERPROBE [probeUnit]
                ((), plan, st') = runStep (command [action])
            length (botCommands plan) `shouldBe` 1
            botChat plan `shouldBe` []
            length (botDebug plan) `shouldBe` 0
            case botCommands plan of
                [SelfCommand ability us] -> do
                    ability `shouldBe` HARVESTGATHERPROBE
                    map (^. R.tag) us `shouldBe` [1]
                other -> expectationFailure $ "unexpected commands " ++ show (length other)
            let updatedObs = dummyObs st'
            case updatedObs ^. S.rawData . R.units of
                [u] -> do
                    let orders = u ^. R.orders
                    length orders `shouldBe` 1
                    let abilityIds = orders ^.. traverse . R.abilityId
                    abilityIds `shouldBe` [fromIntegral (fromEnum HARVESTGATHERPROBE)]
                other -> expectationFailure $ "unexpected units: " ++ show (length other)

        it "debugText records debug command" $ do
            let point = defMessage & C.x .~ 1 & C.y .~ 2 & C.z .~ 3 :: Point
                (_, plan, _) = runStep (debugText "here" point)
            length (botCommands plan) `shouldBe` 0
            botChat plan `shouldBe` []
            case botDebug plan of
                [DebugText text p] -> do
                    text `shouldBe` T.pack "here"
                    p `shouldBe` point
                other -> expectationFailure $ "unexpected debug commands: " ++ show (length other)

        it "agentChat enqueues chat message" $ do
            let (_, plan, _) = runStep (agentChat "gl hf")
            length (botCommands plan) `shouldBe` 0
            length (botDebug plan) `shouldBe` 0
            botChat plan `shouldBe` [T.pack "gl hf"]

probeUnit :: Unit
probeUnit =
    defMessage
        & R.tag .~ 1
        & R.unitType .~ Units.fromEnum' ProtossProbe
        & R.alliance .~ Self
        & R.pos .~ (defMessage & C.x .~ 10 & C.y .~ 20 & C.z .~ 0)

initialObservation :: Observation
initialObservation =
    defMessage
        & S.rawData
            .~ (defMessage & R.units .~ [probeUnit])

initialGrid :: Grid
initialGrid = gridFromLines ["     ", "     ", "     "]

initialState :: DummyState
initialState = DummyState initialObservation initialGrid

unitAbilities :: UnitAbilities
unitAbilities = HashMap.empty

staticInfo :: StaticInfo
staticInfo =
    StaticInfo
        { gameInfo = defMessage
        , playerInfo = defMessage
        , unitTraits = HashMap.empty
        , heightMap = initialGrid
        , expandsPos = []
        , enemyStartLocation = (0, 0)
        , regionGraph = HashMap.empty
        , regionLookup = HashMap.empty
        , siRegions = HashMap.empty
        , siRegionPathToEnemy = []
        }

runStep :: StepMonad DummyState a -> (a, StepPlan, DummyState)
runStep = runStepM staticInfo unitAbilities initialState
