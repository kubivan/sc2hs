{-# LANGUAGE OverloadedLabels #-}

module TestFixtures (
    mockUnit,
    pointAt,
    mkObservation,
    mkStaticInfo,
    mkDynamicState,
    gridFromFile,
) where

import Army.Army (emptyArmy)
import BotDynamicState (BotDynamicState (..))
import Data.HashMap.Strict qualified as HashMap
import Data.Map qualified as Map
import Data.ProtoLens (defMessage)
import Data.Word (Word32, Word64)
import Observation (Cost (..), Observation)
import SC2.Grid (Grid, gridFromLines)
import SC2.Ids.UnitTypeId (UnitTypeId)
import SC2.Proto.Data (Alliance, Point)
import StepMonad (StaticInfo (..))
import System.Random (mkStdGen)
import Units (Unit, fromEnum')

import Lens.Micro ((&), (.~))

pointAt :: Float -> Float -> Float -> Point
pointAt x y z = defMessage & #x .~ x & #y .~ y & #z .~ z

mockUnit :: Word64 -> UnitTypeId -> Alliance -> (Float, Float, Float) -> Unit
mockUnit tag unitType alliance (x, y, z) =
    defMessage
        & #tag .~ tag
        & #unitType .~ fromEnum' unitType
        & #alliance .~ alliance
        & #buildProgress .~ 1
        & #pos .~ pointAt x y z

mkObservation :: [Unit] -> Word32 -> Observation
mkObservation units gameLoop =
    defMessage
        & #rawData . #units .~ units
        & #gameLoop .~ gameLoop

mkStaticInfo :: Grid -> StaticInfo
mkStaticInfo grid =
    StaticInfo
        { gameInfo = defMessage
        , playerInfo = defMessage
        , unitTraits = HashMap.empty
        , heightMap = grid
        , expandsPos = []
        , startLocation = (0, 0)
        , enemyStartLocation = (0, 0)
        , siAsyncStaticInfo = Nothing
        }

mkDynamicState :: Observation -> Grid -> BotDynamicState
mkDynamicState obs grid =
    BotDynamicState
        { dsObs = obs
        , dsGrid = grid
        , dsReservedCost = Cost 0 0
        , dsRandGen = mkStdGen 7
        , dsArmy = emptyArmy
        , dsIntents = Map.empty
        }

gridFromFile :: FilePath -> IO Grid
gridFromFile filePath = gridFromLines . lines <$> readFile filePath
