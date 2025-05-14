
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module SC2.Army.FSM where

import SC2.Army.Behavior
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
import SC2.Army.Army (ArmySquad)

-- squadUpdateState :: (HasArmy d, AgentDynamicState d) => ArmySquad -> StepMonad d ()
-- squadUpdateState s =
--     case squadState s of
--         forming@(StateSquadForming{}) -> squadForm s forming
--         explore@(StateExploreRegion{}) -> squadUpdateStateExploreRegion s explore
--         _ -> return ()

-- squadStep :: (HasArmy d, AgentDynamicState d) => ArmySquad -> StepMonad d ()
-- squadStep s =
--     case squadState s of
--         state@(StateSquadForming{}) -> squadForm s state
--         state@(StateExploreRegion rid region) -> squadExploreRegion s rid region
--         (StateAttack t) -> squadDoAttack s t
--         _ -> return ()

-- agentSquadsStep :: (HasArmy d, AgentDynamicState d) => StepMonad d ()
-- agentSquadsStep = do
--     ds <- agentGet
--     let
--         army = getArmy ds
--         squads = armySquads army

--     mapM_ squadStep squads

data AnyState u where
  AnyState :: FS s => s -> AnyState u

data StepResult s
  = Stay
  | TransitionTo (AnyState (Owner s))

class FS s where
    type Owner s

    fsStep :: (HasArmy d, AgentDynamicState d) => Owner s -> s -> StepMonad d ()
    fsUpdate :: (HasArmy d, AgentDynamicState d) => Owner s -> s -> StepMonad d Bool

    fsOnEnter :: (HasArmy d, AgentDynamicState d) => Owner s -> s -> StepMonad d ()
    fsOnExit :: (HasArmy d, AgentDynamicState d) => Owner s -> s -> StepMonad d ()

data FSSquadIdle = FSSquadIdle

instance FS FSSquadIdle where
    type Owner FSSquadIdle = ArmySquad

    fsStep s _ = traceM "[step] idle" >> wanderAround 10
    fsUpdate _ _ = pure True
    fsOnEnter s _ = traceM $ "[enter] Idle " ++ show (squadId s)
    fsOnExit  s _ = traceM $ "[exit] Idle " ++ show (squadId s)

type SquadFormation = (TilePos, Footprint)
newtype FSSquadForming = FSSquadForming (Maybe SquadFormation)

isSquadFormed :: (HasArmy d, AgentDynamicState d) => ArmySquad -> TilePos -> Footprint -> StepMonad d Bool
isSquadFormed squad center formation = do
        ds <- agentGet
        let army = getArmy ds
            unitByTag t = HashMap.lookup t (armyUnits army)
            -- TODO: it shouldn't happen: updateArmy had to remove dead units from squads
            (leader : units) = catMaybes $ [unitByTag t | t <- squadUnits squad]
            -- filter out leader 'c' : leader goes to center
            unitsFormationPos = (\(dx, dy, _) -> center + (dx, dy)) <$> filter (\(_, _, ch) -> isDigit ch) (pixels formation)

            unitsWithPos = take (length units) unitsFormationPos `zip` units
        return $ all (\(p, u) -> 2 >= distManhattan p (tilePos . view #pos $ u)) unitsWithPos

instance FS FSSquadForming where
    type Owner FSSquadForming = ArmySquad

    fsStep s (FSSquadForming Nothing) = pure ()
    fsStep s (FSSquadForming (Just (fcenter, formation))) = do
         traceM "[step] idle"
         void $ squadMoveToFormation s fcenter formation
    fsUpdate s (FSSquadForming Nothing) = do isSquadFull s
    fsUpdate s (FSSquadForming(Just (center, formation))) = do
        isFull <- isSquadFull s
        isFormed <- isSquadFormed s center formation
        return $ isFull && isFormed

    fsOnEnter squad (FSSquadForming Nothing) = do
        traceM $ "[enter] FSSquadForming " ++ show (squadId squad)
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
            Nothing -> pure ()
            (Just fcenter) -> squadTransitionToGatherToFormation squad fcenter formation
    fsOnEnter s _ = pure ()
    fsOnExit s (FSSquadForming f)  = do
        traceM $ "[exit] FSSquadForming " ++ show (squadId s)
        case f of
            Nothing -> pure ()
            Just (center, fprint) -> void $ removeMarkSM fprint center


data FSExploreRegion = FSExploreRegion RegionId Region

instance FS FSExploreRegion where
    type Owner FSExploreRegion = ArmySquad

    fsStep s (FSExploreRegion rid region) = do
        traceM ("[step] FSExploreRegion " ++ show (squadId s)) >> squadExploreRegion s region
    fsUpdate squad (FSExploreRegion rid region)
        | Set.size region == 0 = return True
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

            agentPut $ setArmy army' ds
            return False
    fsOnEnter s _ = traceM $ "[enter] FSExploreRegion " ++ show (squadId s)
    fsOnExit  s _ = traceM $ "[exit] FSExploreRegion " ++ show (squadId s)

data FSAttack = FSAttack Target
data FSEvade = FSEvade

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
