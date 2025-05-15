module SC2.Army.Behavior where

import Actions (Action (..), UnitTag)
import Observation
-- import SC2.Army.Army
import SC2.Army.Class
import SC2.Army.Utils
import SC2.Army.Squad
import SC2.Geometry
import SC2.Grid
import SC2.Ids.AbilityId
import SC2.Ids.UnitTypeId
import SC2.Proto.Data (Alliance (..), Point, Point2D)
import SC2.Proto.Data qualified as Proto
import StepMonad
import StepMonadUtils
import Units
import Utils

import Conduit (filterC, mapC)
import Control.Applicative ((<|>))
import Control.Monad (filterM, void, when)
import Data.Char (isDigit)
import Data.Foldable qualified as Seq
import Data.Function (on)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.List (minimumBy, partition)
import Data.List.Split (chunksOf)
import Data.Maybe (catMaybes, fromJust, isJust, isNothing, mapMaybe)
import Data.Ord (comparing)
import Data.Set (Set)
import Data.Set qualified as Set
import Lens.Micro ((^.))
import Lens.Micro.Extras (view)
import Safe (headMay, minimumByMay)
import System.Random (StdGen, randomR)

import Debug.Trace (traceM)
import Footprint
import SC2.Proto.Data qualified as Proto

--TODO: implement
--wanderAround :: FS s => s Int -> StepMonad d s
wanderAround s radius = pure ()

-- command to move units to formation. returns true when complete
squadMoveToFormation :: (HasArmy d, AgentDynamicState d) => Squad -> TilePos -> Footprint -> StepMonad d Bool
squadMoveToFormation squad center@(cx, cy) (Footprint formation) = do
    ds <- agentGet
    let unitByTag t = HashMap.lookup t (getUnitMap ds)
        -- TODO: it shouldn't happen: updateArmy had to remove dead units from squads
        (leader : units) = catMaybes $ [unitByTag t | t <- squadUnits squad]
        -- filter out leader 'c' : leader goes to center
        unitsFormationPos = (\(dx, dy, _) -> center + (dx, dy)) <$> filter (\(_, _, ch) -> isDigit ch) formation

        unitsWithPos = take (length units) unitsFormationPos `zip` units

    -- if all (\(p, u) -> p == (tilePos . view #pos $ u) ) unitsWithPos
    if all (\(p, u) -> 2 >= distManhattan p (tilePos . view #pos $ u)) unitsWithPos
        then return True
        else do
            command [PointCommand AttackAttack [leader] (toPoint2D center)]
            command [PointCommand AttackAttack [u] (toPoint2D p) | (p, u) <- unitsWithPos]
            return False

squadExploreRegion :: (HasArmy d, AgentDynamicState d) => Squad -> Region -> StepMonad d ()
squadExploreRegion s region =
    do
        ds <- agentGet
        let unitByTag t = fromJust $ HashMap.lookup t (getUnitMap ds)
            grid = getGrid ds
            targetPos = head $ Set.toList region
            unitTags@(squadLeaderTag : squadsRest) = squadUnits s
            leaderPos = tilePos . view #pos . unitByTag $ squadLeaderTag

            GridBfsRes isFound _ path = gridBfs grid leaderPos (getAllNotSharpNeighbors grid) (== targetPos) (const False)
            posToGo = fromJust $ backoffList path 3

        if isNothing isFound
            then void $ traceM ("[warn] squadExploreRegion: unreacheble: " ++ show targetPos)
            else do
                command [PointCommand AttackAttack [unitByTag ut | ut <- unitTags] (toPoint2D posToGo)]

squadDoAttack :: Squad -> Target -> StepMonad d ()
squadDoAttack squad target = return ()

isSquadFormed :: (HasArmy d, AgentDynamicState d) => Squad -> TilePos -> Footprint -> StepMonad d Bool
isSquadFormed squad center formation = do
        ds <- agentGet
        let unitByTag t = HashMap.lookup t (getUnitMap ds)
            -- TODO: it shouldn't happen: updateArmy had to remove dead units from squads
            (leader : units) = catMaybes $ [unitByTag t | t <- squadUnits squad]
            -- filter out leader 'c' : leader goes to center
            unitsFormationPos = (\(dx, dy, _) -> center + (dx, dy)) <$> filter (\(_, _, ch) -> isDigit ch) (pixels formation)

            unitsWithPos = take (length units) unitsFormationPos `zip` units
        return $ all (\(p, u) -> 2 >= distManhattan p (tilePos . view #pos $ u)) unitsWithPos