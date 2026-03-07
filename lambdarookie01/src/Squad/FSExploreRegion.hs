
module Squad.FSExploreRegion where

import SC2.Utils
import SC2.Grid
import Squad.Class
import Squad.Squad
import Squad.State
import Squad.Behavior
import SC2.Geometry
import StepMonad
import StepMonadUtils
import Actions (Action (..), UnitTag)
import SC2.Ids.AbilityId

import Data.HashMap.Strict qualified as HashMap
import Data.Maybe
import Control.Monad
import Data.Set qualified as Set
import Lens.Micro ((^.))
import Lens.Micro.Extras (view)

import Debug.Trace (traceM)
import Footprint

import Data.Char (isDigit)

-- ---------------------------------------------------------------------------
-- Step

exploreRegionStep :: (HasArmy d, HasGrid d, HasObs d) => FSMSquad SquadState -> Region -> StepMonad d ()
exploreRegionStep s region = squadExploreRegion s region

-- ---------------------------------------------------------------------------
-- Update

exploreRegionUpdate :: (HasArmy d, HasObs d, HasGrid d) => FSMSquad SquadState -> RegionId -> Region -> StepMonad d (Bool, SquadState)
exploreRegionUpdate squad rid region
    | Set.size region == 0 = return (True, SSExploreRegion rid region)
    | otherwise = do
        ds <- agentGet
        let unitByTag t = HashMap.lookup t (getUnitMap ds)
            units = catMaybes $ [unitByTag t | t <- squadUnits squad]

        pixelsToRemove <- fmap concat $ forM units $ \u -> do
            sightRange <- siUnitSightRange u
            return $ tilesInRadius (floor sightRange) (tilePos (u ^. #pos))

        let region' = foldl' (flip Set.delete) region pixelsToRemove
            state' = SSExploreRegion rid region'

        return (Set.size region' == 0, state')

-- ---------------------------------------------------------------------------
-- Enter / Exit / Transition

exploreRegionOnEnter :: (HasArmy d) => FSMSquad SquadState -> StepMonad d ()
exploreRegionOnEnter s = traceM $ "[enter] FSExploreRegion " ++ show (squadId s)

exploreRegionOnExit :: (HasArmy d) => FSMSquad SquadState -> StepMonad d ()
exploreRegionOnExit s = traceM $ "[exit] FSExploreRegion " ++ show (squadId s)

exploreRegionTransitionNext :: (HasArmy d) => FSMSquad SquadState -> StepMonad d SquadState
exploreRegionTransitionNext _ = pure SSIdle
