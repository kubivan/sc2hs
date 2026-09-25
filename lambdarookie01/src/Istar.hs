{-# LANGUAGE GADTs #-}

module Istar where

import Actions
import Conduit
import Conduit (filterC, mapC)
import Control.Monad.Extra (when)
import Data.Conduit ((.|))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Maybe (fromJust, isJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector.Unboxed qualified as VU
import Data.Word
import Debug.Trace (traceM)
import Lens.Micro ((%~), (^.))
import Lens.Micro.Type (Lens')
import Observation (obsUnitsC, unitsSelf)
import SC2.Grid.Algo (RegionId)
import SC2.Grid.Core (Grid)
import SC2.Ids.Ids
import SC2.Ids.UnitTypeId (UnitTypeId)
import SC2.Proto.Data (Alliance (..))
import SC2.Proto.Data qualified as Proto
import SC2.Spatial (Spatial (..), distManhattan)
import SC2.TilePos (TilePos)
import SC2.Utils (isArmyUnit)
import StepMonad
  ( AsyncStaticInfo (..)
  , HasObs
  , StaticInfo (..)
  , StepMonad
  , agentGet
  , agentModify
  , agentObs
  , agentStatic
  , command
  )
import Units (Unit, allianceC, isBuilding, runC, unitIdleC, unitTypeC, unitTypeId)
import Utils (unitIsHarvesting)

type VisionDecay = (Int, Int, VU.Vector Word64)

visionDecayFromGrid :: Grid -> VisionDecay
visionDecayFromGrid (w, h, _) = (w, h, VU.replicate (w * h) maxDecay)

visionDecayEmpty = (0, 0, VU.fromList [])

maxDecay :: Word64
maxDecay = maxBound

data ScoutState
  = ScoutStateIdle
  | ScoutStateMove Unit TilePos
  | ScoutStateRetreat Unit TilePos
  | ScoutStateScouting Unit RegionId
  deriving (Eq, Show)

data ScoutTask = ScoutTask
  { scoutUnit :: Unit
  , scoutRegionId :: Int
  }
  deriving (Eq, Show)

stepScouting :: (HasIstar d, HasObs d) => StepMonad d ScoutState
stepScouting = do
  istar <- getIstar

  si <- agentStatic
  obs <- agentObs
  let masi = siAsyncStaticInfo si
      scoutState = istarScoutState istar
  traceM $ "IstarScouting: " ++ show scoutState
  if isJust masi
    then do
      let asi = fromJust masi
          enemyBaseRegionId = last $ asiRegionPathToEnemy asi
          enemyBaseRegion = asiRegions asi HashMap.! enemyBaseRegionId
          enemyBaseRegionPos = Set.findMin enemyBaseRegion

          units = unitsSelf obs
      case scoutState of
        ScoutStateIdle -> do
          let probe = head $ runC $ units .| unitTypeC ProtossProbe .| filterC unitIsHarvesting
          pure $ ScoutStateMove probe enemyBaseRegionPos
        ScoutStateMove scout destPos -> do
          let mu = runConduitPure $ units .| filterC (\u -> scout ^. #tag == u ^. #tag) .| headC
          case mu of
            Nothing -> pure ScoutStateIdle
            Just unitAlive -> do
              if unitAlive ^. #shield < unitAlive ^. #shieldMax
                then
                  pure $ ScoutStateRetreat unitAlive (startLocation si)
                else do
                  let dist = distManhattan unitAlive destPos
                  traceM $ "!! dist to " ++ show dist
                  if dist < 20
                    then
                      pure $ ScoutStateRetreat unitAlive (startLocation si)
                    else
                      pure $ ScoutStateMove unitAlive destPos
        ScoutStateRetreat scout destPos -> do
          let mu = runConduitPure $ units .| filterC (\u -> scout ^. #tag == u ^. #tag) .| headC
          case mu of
            Nothing -> pure ScoutStateIdle
            Just unitAlive -> do
              let dist = distManhattan unitAlive destPos
              traceM $ "!! dist to " ++ show dist
              if dist < 20
                then
                  pure $ ScoutStateMove unitAlive enemyBaseRegionPos
                else
                  pure ScoutStateIdle
        _ -> pure scoutState
    else
      pure ScoutStateIdle

data IstarState = IstarState
  { istarSeenEnemies :: Set UnitTypeId
  , istarSeenBuildings :: Set UnitTypeId
  , istarVisionDecay :: VisionDecay
  , istarScoutState :: ScoutState
  }

class HasIstar d where
  scoutingL :: Lens' d IstarState

getIstar :: (HasIstar d) => StepMonad d IstarState
getIstar = (^. scoutingL) <$> agentGet

commandScouting :: (HasObs d) => ScoutState -> StepMonad d ()
commandScouting scoutingState = do
  case scoutingState of
    ScoutStateMove u dest -> command [PointCommand MOVE [u] (toPoint2D dest)]
    -- ScoutStateScouting r RegionId
    _ -> pure ()

modifyIstar ::
  (HasIstar d) =>
  (IstarState -> IstarState) ->
  StepMonad d ()
modifyIstar f = agentModify (scoutingL %~ f)

istarEmpty :: IstarState
istarEmpty = IstarState Set.empty Set.empty visionDecayEmpty ScoutStateIdle

stepIstar :: (StepMonad.HasObs d, HasIstar d) => StepMonad d ()
stepIstar = do
  istar <- getIstar
  obs <- agentObs
  let enemies =
        Set.fromList $
          runC $
            obsUnitsC obs .| allianceC Enemy .| filterC isArmyUnit .| mapC unitTypeId
      enemyBuildings =
        Set.fromList $
          runC $
            obsUnitsC obs .| allianceC Enemy .| filterC isBuilding .| mapC unitTypeId
  scoutState' <- stepScouting
  commandScouting scoutState'
  modifyIstar $
    const
      ( istar
          { istarSeenEnemies = istarSeenEnemies istar `Set.union` enemies
          , istarSeenBuildings = istarSeenBuildings istar `Set.union` enemyBuildings
          , istarScoutState = scoutState'
          }
      )

type UnitComposition = [UnitTypeId]

-- counter

-- data UnitComposition = UnitComposition
--   { uc
--
--   }
