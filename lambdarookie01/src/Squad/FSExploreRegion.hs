module Squad.FSExploreRegion where

import Actions (Action (..), UnitTag)
import SC2.Geometry
import SC2.Grid
import SC2.Ids.AbilityId
import SC2.Utils
import SC2.Spatial
import Squad.Behavior
import Squad.FSMLog
import Squad.Squad
import Squad.State
import StepMonad
import StepMonadUtils

import Control.Monad
import Data.Set qualified as Set
import Lens.Micro ((^.))
import Lens.Micro.Extras (view)

import Footprint

import Army.Class (HasArmy)
import SquadUtils (squadUnits)

-- ---------------------------------------------------------------------------
-- Step

exploreRegionStep ::
  (HasArmy d, HasGrid d, HasObs d) => FSMSquad SquadState -> FSExploreRegion -> StepMonad d ()
exploreRegionStep s (FSExploreRegion _ region) = squadExploreRegion s region

-- ---------------------------------------------------------------------------
-- Update

exploreRegionUpdate ::
  (HasArmy d, HasObs d, HasGrid d) =>
  FSMSquad SquadState -> FSExploreRegion -> StepMonad d UpdateResult
exploreRegionUpdate squad st@(FSExploreRegion rid region)
  | Set.size region == 0 = return (Transition SSIdle)
  | otherwise = do
      units <- squadUnits squad

      pixelsToRemove <- fmap concat $ forM units $ \u -> do
        sightRange <- siUnitSightRange u
        return $ tilesInRadius (floor sightRange) (tilePos (u ^. #pos))

      let region' = foldl' (flip Set.delete) region pixelsToRemove
          state' = FSExploreRegion rid region'

      return $
        if Set.size region' == 0
          then Transition SSIdle
          else Continue (SSExploreRegion state')

-- ---------------------------------------------------------------------------
-- Enter / Exit / Transition

exploreRegionOnEnter :: (HasArmy d) => FSMSquad SquadState -> StepMonad d ()
exploreRegionOnEnter squad = traceFSM squad "enter"

exploreRegionOnExit :: (HasArmy d) => FSMSquad SquadState -> StepMonad d ()
exploreRegionOnExit squad = traceFSM squad "exit"
