{-# LANGUAGE GADTs #-}

module Istar where

import Actions
import Conduit
import Conduit (filterC, mapC)
import Control.Monad.Extra (when)
import Control.Monad.Trans.Maybe
import Data.Conduit ((.|))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Maybe (fromMaybe, isJust, listToMaybe)
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
  , MaybeStepMonad
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

data ScoutTask
  = ScoutTaskIdle
  | ScoutTaskMove TilePos
  | ScoutTaskRetreat TilePos
  | ScoutTaskScout RegionId (Set TilePos)
  deriving (Eq, Show)

data ScoutContext = ScoutContext
  { scoutUnit :: Maybe Unit
  , scoutTask :: ScoutTask
  }
  deriving (Eq, Show)

getJust :: (Applicative m) => Maybe a -> MaybeT m a
getJust = MaybeT . pure

stepScouting :: (HasIstar d, HasObs d) => MaybeStepMonad d ScoutContext
stepScouting = do
  istar <- lift getIstar
  si <- lift agentStatic
  obs <- lift agentObs
  asi <- getJust $ siAsyncStaticInfo si
  enemyBaseRegionId <- getJust $ listToMaybe $ reverse $ asiRegionPathToEnemy asi
  enemyBaseRegion <- getJust $ HashMap.lookup enemyBaseRegionId $ asiRegions asi
  enemyBaseRegionPos <- getJust $ Set.lookupMin enemyBaseRegion
  let scoutContext = istarScoutContext istar
      units = unitsSelf obs
      findScout scout =
        runConduitPure $
          units .| filterC (\unit -> scout ^. #tag == unit ^. #tag) .| headC
  traceM $ "IstarScouting: " ++ show scoutContext
  case scoutTask scoutContext of
    ScoutTaskIdle -> do
      probe <- getJust $ listToMaybe $ runC $ units .| unitTypeC ProtossProbe .| filterC unitIsHarvesting
      pure $
        ScoutContext
          { scoutUnit = Just probe
          , scoutTask = ScoutTaskMove enemyBaseRegionPos
          }
    ScoutTaskMove destPos -> do
      scout <- getJust $ scoutUnit scoutContext
      unitAlive <- getJust $ findScout scout
      if unitAlive ^. #shield < unitAlive ^. #shieldMax
        then
          pure $
            ScoutContext
              { scoutUnit = Just unitAlive
              , scoutTask = ScoutTaskRetreat (startLocation si)
              }
        else do
          let dist = distManhattan unitAlive destPos
          traceM $ "!! dist to " ++ show dist
          if dist < 20
            then
              pure $
                ScoutContext
                  { scoutUnit = Just unitAlive
                  , scoutTask = ScoutTaskRetreat (startLocation si)
                  }
            else pure $ ScoutContext{scoutUnit = Just unitAlive, scoutTask = ScoutTaskMove destPos}
    ScoutTaskRetreat destPos -> do
      scout <- getJust $ scoutUnit scoutContext
      unitAlive <- getJust $ findScout scout
      if distManhattan unitAlive destPos < 20
        then
          pure $
            ScoutContext
              { scoutUnit = Just unitAlive
              , scoutTask = ScoutTaskMove enemyBaseRegionPos
              }
        else pure $ ScoutContext{scoutUnit = Just unitAlive, scoutTask = ScoutTaskRetreat destPos}
    _ -> pure scoutContext

data IstarState = IstarState
  { istarSeenEnemies :: Set UnitTypeId
  , istarSeenBuildings :: Set UnitTypeId
  , istarVisionDecay :: VisionDecay
  , istarScoutContext :: ScoutContext
  }

class HasIstar d where
  scoutingL :: Lens' d IstarState

getIstar :: (HasIstar d) => StepMonad d IstarState
getIstar = (^. scoutingL) <$> agentGet

commandScouting :: (HasObs d) => Maybe ScoutContext -> StepMonad d ()
commandScouting Nothing = pure ()
commandScouting (Just (ScoutContext (Just u) (ScoutTaskMove dest))) = command [PointCommand MOVE [u] (toPoint2D dest)]
commandScouting (Just (ScoutContext (Just u) (ScoutTaskRetreat dest))) = command [PointCommand MOVE [u] (toPoint2D dest)]
commandScouting _ = pure ()

modifyIstar ::
  (HasIstar d) =>
  (IstarState -> IstarState) ->
  StepMonad d ()
modifyIstar f = agentModify (scoutingL %~ f)

istarEmpty :: IstarState
istarEmpty = IstarState Set.empty Set.empty visionDecayEmpty (ScoutContext Nothing ScoutTaskIdle)

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
  scoutState' <- runMaybeT stepScouting
  commandScouting scoutState'
  modifyIstar $
    const
      ( istar
          { istarSeenEnemies = istarSeenEnemies istar `Set.union` enemies
          , istarSeenBuildings = istarSeenBuildings istar `Set.union` enemyBuildings
          , istarScoutContext = fromMaybe (istarScoutContext istar) scoutState'
          }
      )

type UnitComposition = [UnitTypeId]

-- counter

-- data UnitComposition = UnitComposition
--   { uc
--
--   }
