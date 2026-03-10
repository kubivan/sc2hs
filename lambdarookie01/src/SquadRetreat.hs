
module SquadRetreat where

import Squad.Squad
import Squad.State
import Squad.Class
import SC2.Geometry
import Observation
import SC2.Grid
import StepMonad
import Actions (Action (PointCommand), UnitTag)
import SC2.Ids.AbilityId (AbilityId (ATTACKATTACK))

import Data.HashMap.Strict qualified as HashMap
import Data.Maybe (catMaybes, listToMaybe, fromJust)
import Data.Set qualified as Set
import Lens.Micro ((^.))
import Debug.Trace (traceM)

-- ---------------------------------------------------------------------------
-- Step

retreatStep :: (HasArmy d, HasObs d, HasGrid d) => FSMSquad SquadState -> Maybe TilePos -> StepMonad d ()
retreatStep squad Nothing = error("retreatStep for Nothing rallypoint shouldnt happen")
retreatStep squad (Just rallyPos) = do
    ds <- agentGet
    let unitByTag t = HashMap.lookup t (getUnitMap ds)
        units = catMaybes [unitByTag t | t <- squadUnits squad]
    if null units
        then pure ()
        else command [PointCommand ATTACKATTACK units (toPoint2D rallyPos)]

-- ---------------------------------------------------------------------------
-- Update

findRetreatPoint :: (HasArmy d, HasObs d, HasGrid d) => FSMSquad SquadState -> StepMonad d TilePos
findRetreatPoint squad = do
  ds <- agentGet
  si <- agentStatic
  case siAsyncStaticInfo si of
    Nothing -> return $ startLocation si
    Just asi -> do
      obs <- agentObs
      let unitByTag t = HashMap.lookup t (getUnitMap ds)
          leader = fromJust $ unitByTag (head (squadUnits squad))
          leaderPos = tilePos (leader ^. #pos)
          nexusPos = tilePos $ findNexus obs ^. #pos
          -- nexusPos = head (playerStartPos si)
          regionLookup = asiRegionLookup asi
          leaderRegion = fromJust $ HashMap.lookup leaderPos regionLookup
          startRegion = fromJust $ HashMap.lookup nexusPos regionLookup
          rg = asiRegionGraph asi
          pathToHome = regionGraphBfs rg leaderRegion startRegion
          retreatRegionId = head pathToHome

          region = fromJust $ HashMap.lookup retreatRegionId (asiRegions asi)

      return $ Set.findMin region


retreatUpdate :: (HasArmy d, HasObs d, HasGrid d) => FSMSquad SquadState -> Maybe TilePos -> StepMonad d UpdateResult
retreatUpdate squad Nothing = do 
  pos <- findRetreatPoint squad
  return (Continue (SSRetreat (Just pos)))

retreatUpdate squad st@(Just rallyPos) = do
    ds <- agentGet
    let unitByTag t = HashMap.lookup t (getUnitMap ds)
        leader = fromJust $ unitByTag (head (squadUnits squad))
        arrived = distManhattan (tilePos (leader ^. #pos)) rallyPos <= 3
        healed = leader ^. #shield  == leader ^. #shieldMax

    pure $ if arrived || healed
        then Transition SSIdle
        else Continue (SSRetreat st)

-- ---------------------------------------------------------------------------
-- Enter / Exit / Transition

retreatOnEnter :: (HasArmy d) => FSMSquad SquadState -> StepMonad d ()
retreatOnEnter squad = traceM $ "[enter] SquadRetreat " ++ show (squadId squad)

retreatOnExit :: (HasArmy d) => FSMSquad SquadState -> StepMonad d ()
retreatOnExit squad = traceM $ "[exit] SquadRetreat " ++ show (squadId squad)

