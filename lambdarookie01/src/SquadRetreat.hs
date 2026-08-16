module SquadRetreat where

import Actions (Action (PointCommand))
import Army.Class
import Observation
import SC2.Geometry
import SC2.Grid
import SC2.Ids.AbilityId (AbilityId (ATTACKATTACK))
import SC2.Spatial qualified as Spatial
import Squad.FSMLog
import Squad.Squad
import Squad.State
import StepMonad

import Army.Army (armyByTag)
import Data.HashMap.Strict qualified as HashMap
import Data.Maybe (fromJust, fromMaybe, listToMaybe)
import Data.Set qualified as Set
import Lens.Micro ((^.))
import SquadUtils (squadUnits)

-- ---------------------------------------------------------------------------
-- Step

retreatStep ::
  (HasArmy d, HasObs d, HasGrid d) => FSMSquad SquadState -> Maybe TilePos -> StepMonad d ()
retreatStep _ Nothing = error "retreatStep for Nothing rallypoint shouldnt happen"
retreatStep squad (Just rallyPos) = do
  units <- squadUnits squad
  if null units
    then pure ()
    else command [PointCommand ATTACKATTACK units (fromTuple rallyPos)]

-- ---------------------------------------------------------------------------
-- Update

findRetreatPoint ::
  (HasArmy d, HasObs d, HasGrid d) =>
  FSMSquad SquadState ->
  StepMonad d TilePos
findRetreatPoint squad = do
  ds <- agentGet
  si <- agentStatic
  obs <- agentObs

  let fallback = startLocation si

      retreatPoint = do
        asi <- siAsyncStaticInfo si
        unitTag <- listToMaybe (squadTags squad)
        leader <- HashMap.lookup unitTag (getUnitMap ds)
        nexus <- findNexus obs

        let leaderPos = tilePos (leader ^. #pos)
            nexusPos = tilePos $ nexus ^. #pos
            regionLookup = asiRegionLookup asi

        leaderRegion <- HashMap.lookup leaderPos regionLookup
        startRegion <- HashMap.lookup nexusPos regionLookup

        let pathToHome =
              regionGraphBfs
                (asiRegionGraph asi)
                leaderRegion
                startRegion

        retreatRegionId <- listToMaybe pathToHome
        region <- HashMap.lookup retreatRegionId (asiRegions asi)

        Set.lookupMin region

  return $ fromMaybe fallback retreatPoint

retreatUpdate ::
  (HasArmy d, HasObs d, HasGrid d) => FSMSquad SquadState -> Maybe TilePos -> StepMonad d UpdateResult
retreatUpdate squad Nothing = do
  pos <- findRetreatPoint squad
  return (Continue (SSRetreat (Just pos)))
retreatUpdate squad st@(Just rallyPos) = do
  leader <- fromJust <$> armyByTag (head . squadTags $ squad)

  let arrived = Spatial.distManhattan (tilePos (leader ^. #pos)) rallyPos <= 3
      healed = leader ^. #shield == leader ^. #shieldMax

  pure $
    if arrived || healed
      then Transition SSIdle
      else Continue (SSRetreat st)

-- ---------------------------------------------------------------------------
-- Enter / Exit / Transition

retreatOnEnter :: (HasArmy d) => FSMSquad SquadState -> StepMonad d ()
retreatOnEnter squad = traceFSM squad "enter"

retreatOnExit :: (HasArmy d) => FSMSquad SquadState -> StepMonad d ()
retreatOnExit squad = traceFSM squad "exit"
