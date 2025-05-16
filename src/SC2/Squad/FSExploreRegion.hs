
module SC2.Squad.FSExploreRegion where

import SC2.Utils
import SC2.Grid
import SC2.Squad.Class
import SC2.Squad.Squad
import SC2.Squad.Types
import SC2.Geometry
import StepMonad
import StepMonadUtils
import Actions (Action (..), UnitTag)
import SC2.Ids.AbilityId

import Control.Monad (void)
import Data.HashMap.Strict qualified as HashMap
import Data.Maybe
import Data.Set qualified as Set
import Lens.Micro ((^.))
import Lens.Micro.Extras (view)

import Debug.Trace (traceM)
import Footprint

import Data.Char (isDigit)

data FSExploreRegion = FSExploreRegion RegionId Region

instance SquadFS FSExploreRegion where

    fsStep s (FSExploreRegion rid region) = do
        traceM ("[step] FSExploreRegion " ++ show (squadId s)) >> squadExploreRegion s region
    fsUpdate squad st@(FSExploreRegion rid region)
        | Set.size region == 0 = return (True, st)
        | otherwise = do
            ds <- agentGet
            let unitByTag t = HashMap.lookup t (getUnitMap ds)
                -- TODO: it shouldn't happen: updateArmy had to remove dead units from squads
                units = catMaybes $ [unitByTag t | t <- squadUnits squad]

                -- TODO: correct radius
                -- TODO: intersect 2 sets instead
                pixelsToRemove = concatMap (tilesInRadius 5) (tilePos . view #pos <$> units)
                region' = foldl' (flip Set.delete) region pixelsToRemove

                state' = FSExploreRegion rid region'

            return (Set.size region' == 0, state')
    fsOnEnter s _ = traceM $ "[enter] FSExploreRegion " ++ show (squadId s)
    fsOnExit  s _ = traceM $ "[exit] FSExploreRegion " ++ show (squadId s)

-- data FSAttack = FSAttack Target
-- data FSEvade = FSEvade