module SC2.Army.SquadFSM where

import Actions (Action (..), UnitTag)
import SC2.Army.Army
import SC2.Army.Class
import SC2.Army.Utils
import SC2.Grid
import SC2.Geometry
import Observation
import SC2.Ids.AbilityId
import SC2.Ids.UnitTypeId
import SC2.Proto.Data (Alliance (..), Point, Point2D)
import SC2.Proto.Data qualified as Proto
import StepMonad
import Units
import Utils

import Conduit (filterC, mapC)
import Control.Applicative ((<|>))
import Control.Monad (filterM, void, when)
import Data.Foldable qualified as Seq
import Data.Function (on)
import Data.List (minimumBy, partition)
import Data.List.Split (chunksOf)
import Data.HashMap.Strict(HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.Set(Set)
import Data.Set qualified as Set
import Data.Maybe (catMaybes, fromJust, isJust, isNothing, mapMaybe)
import Data.Ord (comparing)
import Lens.Micro ((^.))
import Lens.Micro.Extras (view)
import Safe (headMay, minimumByMay)
import System.Random (StdGen, randomR)

import Debug.Trace (traceM)

isSquadFull :: HasArmy d => ArmySquad -> StepMonad d Bool
isSquadFull squad = do
    ds <- agentGet
    let unitMap = armyUnits $ getArmy ds
        tags = squadUnits squad
        squadSize = length tags
    return $ length tags == squadSize && all (`HashMap.member` unitMap) tags

squadAssignedRegion :: ArmySquad -> Maybe RegionId
squadAssignedRegion squad = case squadState squad of
    StateExploreRegion rid _ -> Just rid
    _ -> Nothing

squadAssign :: (HasArmy d) => ArmySquad -> StepMonad d ()
squadAssign s = do
    ds <- agentGet
    (si, _) <- agentAsk
    let regionById rid = siRegions si HashMap.! rid
        allRegions = HashMap.keysSet $ siRegions si
        usedRegions = HashSet.fromList $ mapMaybe squadAssignedRegion (armySquads (getArmy ds))
        availableRegionIds = HashSet.toList $ allRegions `HashSet.difference` usedRegions
    traceM $ "assigning: all regions " ++ show (allRegions)
    traceM $ "assigning: used regions " ++ show (usedRegions)
    traceM $ "assigning: availableRegionIds " ++ show (availableRegionIds)
    case availableRegionIds of
        [] -> return () -- no unassigned regions left
        (rid : _) -> do
            -- Assign squad to region
            let squad' = s{squadState = StateExploreRegion rid (regionById rid)}
                squads' = replaceSquad squad' (armySquads (getArmy ds))
                army' = (getArmy ds){armySquads = squads'}
            traceM $ "   assigded squads': " ++ show squad'
            agentPut $ setArmy army' ds

agentAssignIdleSquads :: HasArmy d => StepMonad d ()
agentAssignIdleSquads = do
    ds <- agentGet
    let army = getArmy ds
        squads = armySquads army

    fullIdleSquads <- filter isSquadIdle <$> filterM isSquadFull squads
    mapM_ squadAssign fullIdleSquads

agentSquadsStep :: (HasArmy d, AgentDynamicState d) => StepMonad d ()
agentSquadsStep = do
    ds <- agentGet
    let
        army = getArmy ds
        squads = armySquads army

    mapM_ squadStep squads

-- data ArmyUnitState = StateIdle | StateExplore Target | StateExploreRegion RegionId Region | StateAttack Target | StackEvade deriving (Eq, Show)
squadExploreRegion :: (HasArmy d, AgentDynamicState d) => ArmySquad -> RegionId -> Region -> StepMonad d ()
squadExploreRegion s rid region =
    do
        traceM $ "exploring " ++ show rid ++ " size: " ++ show (Set.size region)
        ds <- agentGet
        let army = getArmy ds
            unitByTag t = fromJust $ HashMap.lookup t (armyUnits army)
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

squadDoAttack :: ArmySquad -> Target -> StepMonad d ()
squadDoAttack squad target = return ()

squadStep :: (HasArmy d, AgentDynamicState d) => ArmySquad -> StepMonad d ()
squadStep s =
    case squadState s of
        (StateExploreRegion rid region) -> squadExploreRegion s rid region
        (StateAttack t) -> squadDoAttack s t
        _ -> return ()

agentUpdateSquads :: HasArmy d => StepMonad d ()
agentUpdateSquads = do
    ds <- agentGet
    let army = getArmy ds
        squads = armySquads army

    mapM_ squadUpdateState squads

squadSwitchIdle :: HasArmy d => ArmySquad -> StepMonad d ()
squadSwitchIdle s = do
    traceM "switch to idle"
    ds <- agentGet
    let squad' = s{squadState = StateSquadIdle}
        army = getArmy ds
        squads' = replaceSquad squad' (armySquads army)
        army'   = army { armySquads = squads' }
    agentPut $ setArmy army' ds

-- data ArmyUnitState = StateIdle | StateExplore Target | StateExploreRegion RegionId Region | StateAttack Target | StackEvade deriving (Eq, Show)
squadUpdateStateExploreRegion :: HasArmy d => ArmySquad -> RegionId -> Region -> StepMonad d ()
squadUpdateStateExploreRegion s rid region
    | Set.size region == 0 = squadSwitchIdle s
    | otherwise = do
        ds <- agentGet
        let army = getArmy ds
            unitByTag t = HashMap.lookup t (armyUnits army)
            -- TODO: it shouldn't happen: updateArmy had to remove dead units from squads
            units = catMaybes $ [unitByTag t | t <- squadUnits s]

            -- TODO: correct radius
            -- TODO: intersect 2 sets instead
            pixelsToRemove = concatMap (tilesInRadius 5) (tilePos . view #pos <$> units)
            region' = foldl' (flip Set.delete) region pixelsToRemove

            squad' = s{squadState = StateExploreRegion rid region'}
            updateSquads = replaceSquad squad' (armySquads (getArmy ds))
            army' = (getArmy ds){armySquads = updateSquads}
        if Set.size region' == 0
            then
                squadSwitchIdle s
            else
                agentPut $ setArmy army' ds

squadUpdateState :: HasArmy d => ArmySquad -> StepMonad d ()
squadUpdateState s =
    case squadState s of
        (StateExploreRegion rid region) -> squadUpdateStateExploreRegion s rid region
        _ -> return ()