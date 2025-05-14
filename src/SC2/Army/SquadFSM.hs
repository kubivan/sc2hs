module SC2.Army.SquadFSM where

import Actions (Action (..), UnitTag)
import Observation
import SC2.Army.Army
import SC2.Army.Class
import SC2.Army.Utils
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

agentAssignIdleSquads :: (HasArmy d) => StepMonad d ()
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

squadForm :: (HasArmy d, AgentDynamicState d) => ArmySquad -> SquadState -> StepMonad d ()
squadForm squad (StateSquadForming Nothing) = do
    isFull <- isSquadFull squad
    when isFull $ do
        traceM "squadForm: squadIsFull"
        ds <- agentGet
        let army = getArmy ds
            formation = squadFormationFootprint
            unitByTag t = HashMap.lookup t (armyUnits army)
            -- TODO: it shouldn't happen: updateArmy had to remove dead units from squads
            units = catMaybes $ [unitByTag t | t <- squadUnits squad]
            leader = head units
            leaderTpos = tilePos $ leader ^. #pos
        -- TODO: magic number: consider implement findplacementinregion
        gatherPlace <- findPlacementPointInRadiusSM formation leaderTpos 10
        case gatherPlace of
            -- Nothing -> traceM ("[err] cannot gather for some reason: move separately") $ squadTransitionToExploreMove squad
            Nothing -> squadTransitionToIdle squad
            (Just fcenter) -> squadMoveToFormation squad fcenter formation >> squadTransitionToGatherToFormation squad fcenter formation
squadForm squad (StateSquadForming (Just (cpos, formation))) = do
    traceM "squadForm: moving to the formation"
    isComplete <- squadMoveToFormation squad cpos formation
    when isComplete $ removeMarkSM formation cpos >> squadTransitionToIdle squad
squadForm _ s = error ("squadForm: invalid state: " ++ show s)

squadStep :: (HasArmy d, AgentDynamicState d) => ArmySquad -> StepMonad d ()
squadStep s =
    case squadState s of
        state@(StateSquadForming{}) -> squadForm s state
        state@(StateExploreRegion rid region) -> squadExploreRegion s rid region
        (StateAttack t) -> squadDoAttack s t
        _ -> return ()

agentUpdateSquads :: (HasArmy d, AgentDynamicState d) => StepMonad d ()
agentUpdateSquads = do
    ds <- agentGet
    let army = getArmy ds
        squads = armySquads army

    mapM_ squadUpdateState squads

squadTransitionTo :: (HasArmy d) => ArmySquad -> SquadState -> StepMonad d ()
squadTransitionTo squad state' = do
    traceM $ "squad " ++ show (squadId squad) ++ "switching to " ++ armyUnitStateStr state'
    agentModify $ \ds ->
        let squad' = squad{squadState = state'}
            army = getArmy ds
            army' = army{armySquads = replaceSquad squad' (armySquads army)}
         in setArmy army' ds

squadTransitionToIdle :: (HasArmy d) => ArmySquad -> StepMonad d ()
squadTransitionToIdle s = squadTransitionTo s StateSquadIdle

squadTransitionToGatherToFormation :: (HasArmy d) => ArmySquad -> TilePos -> Footprint -> StepMonad d ()
squadTransitionToGatherToFormation squad fcenter formation = do
    traceM "switch to squadTransitionToGatherToFormation"
    ds <- agentGet
    let
        state' = StateSquadForming (Just (fcenter, formation))
        squad' = squad{squadState = state'}
        army = getArmy ds
        squads' = replaceSquad squad' (armySquads army)
        army' = army{armySquads = squads'}
    agentPut $ setArmy army' ds

-- data ArmyUnitState = StateIdle | StateExplore Target | StateExploreRegion RegionId Region | StateAttack Target | StackEvade deriving (Eq, Show)
-- squadUpdateStateExploreRegion :: (HasArmy d) => ArmySquad -> RegionId -> Region -> StepMonad d ()
squadUpdateStateExploreRegion :: (HasArmy d) => ArmySquad -> SquadState -> StepMonad d ()
squadUpdateStateExploreRegion squad state@(StateExploreRegion rid region)
    | Set.size region == 0 = squadTransitionToIdle squad
    | otherwise = do
        ds <- agentGet
        let army = getArmy ds
            unitByTag t = HashMap.lookup t (armyUnits army)
            -- TODO: it shouldn't happen: updateArmy had to remove dead units from squads
            units = catMaybes $ [unitByTag t | t <- squadUnits squad]

            -- TODO: correct radius
            -- TODO: intersect 2 sets instead
            pixelsToRemove = concatMap (tilesInRadius 5) (tilePos . view #pos <$> units)
            region' = foldl' (flip Set.delete) region pixelsToRemove

            state' = StateExploreRegion rid region'
            squad' = squad{squadState = state'}
            updateSquads = replaceSquad squad' (armySquads (getArmy ds))
            army' = (getArmy ds){armySquads = updateSquads}
        if Set.size region' == 0
            then
                squadTransitionToIdle squad
            else
                agentPut $ setArmy army' ds
squadUpdateStateExploreRegion _ s = error ("squadUpdateStateExploreRegion: invalid state " ++ show s)

-- command to move units to formation. returns true when complete
squadMoveToFormation :: (HasArmy d, AgentDynamicState d) => ArmySquad -> TilePos -> Footprint -> StepMonad d Bool
squadMoveToFormation squad center@(cx, cy) (Footprint formation) = do
    ds <- agentGet
    let army = getArmy ds
        unitByTag t = HashMap.lookup t (armyUnits army)
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

squadUpdateState :: (HasArmy d, AgentDynamicState d) => ArmySquad -> StepMonad d ()
squadUpdateState s =
    case squadState s of
        forming@(StateSquadForming{}) -> squadForm s forming
        explore@(StateExploreRegion{}) -> squadUpdateStateExploreRegion s explore
        _ -> return ()
