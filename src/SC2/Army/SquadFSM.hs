{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs, ExistentialQuantification, RankNTypes #-}

module SC2.Army.SquadFSM where

import SC2.Army.Behavior
-- import SC2.Army.Army
import SC2.Army.Utils
import SC2.Army.Class
import SC2.Grid
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

data Squad = Squad
  { --squadId     :: Int
    squadUnits  :: [UnitTag]
  , squadState  :: SquadState
  }


squadId :: Squad -> UnitTag
squadId s = head $ squadUnits s

isSquadFull :: (HasArmy d) => Squad -> StepMonad d Bool
isSquadFull squad = do
    ds <- agentGet
    let unitMap = getUnitMap ds
        tags = squadUnits squad
        -- TODO: magic number
        squadSize = 5
    return $ length tags == squadSize && all (`HashMap.member` unitMap) tags && all (\t -> let Just u = HashMap.lookup t unitMap in (1.0 :: Float) == u ^. #buildProgress) tags

data Target
    = TargetPos TilePos
    | TargetUnit UnitTag
    deriving (Eq, Show)

replaceSquad :: Squad -> [Squad] -> [Squad]
replaceSquad new = map (\s -> if squadId s == squadId new then new else s)

class SquadFS st where
    wrapState :: st -> SquadState
    unwrapState :: SquadState -> Maybe st

    fsStep :: (HasArmy d, AgentDynamicState d) => Squad -> st -> StepMonad d ()
    fsUpdate :: (HasArmy d, AgentDynamicState d) => Squad -> st -> StepMonad d (Bool, st)

    fsOnEnter :: (HasArmy d, AgentDynamicState d) => Squad -> st -> StepMonad d ()
    fsOnExit :: (HasArmy d, AgentDynamicState d) => Squad -> st -> StepMonad d ()

data SquadState where
  SquadIdleState      :: FSSquadIdle         -> SquadState
  SquadFormingState   :: FSSquadForming      -> SquadState
  SquadExploreState   :: FSExploreRegion     -> SquadState

isSquadIdle :: Squad -> Bool
isSquadIdle s = case unwrapState (squadState s) of
    Just FSSquadIdle -> True
    Nothing -> False

squadAssignedRegion :: Squad -> Maybe RegionId
squadAssignedRegion squad = case unwrapState(squadState squad) of
    Just (FSExploreRegion rid _) -> Just rid
    Nothing -> Nothing

dispatchUpdate
  :: (HasArmy d, AgentDynamicState d)
  => Squad -> SquadState -> StepMonad d (Bool, SquadState)
dispatchUpdate squad state = case matchState state of
  Just (SomeFS st) -> do
    (done, newSt) <- fsUpdate squad st
    pure (done, wrapState newSt)
  Nothing -> error "Unknown SquadState in dispatchUpdate"

dispatchStep :: (HasArmy d, AgentDynamicState d) => Squad -> SquadState -> StepMonad d ()
dispatchStep squad state = case matchState state of
  Just (SomeFS st) -> fsStep squad st
  Nothing -> error "Unknown SquadState in dispatchStep"

dispatchOnEnter
  :: forall d. (HasArmy d, AgentDynamicState d)
  => Squad -> SquadState -> StepMonad d ()
dispatchOnEnter squad st = case matchState st of
  Just (SomeFS st) -> fsOnEnter squad st
  Nothing -> error "Unknown SquadState in dispatchStep"

dispatchOnExit
  :: forall d. (HasArmy d, AgentDynamicState d)
  => Squad -> SquadState -> StepMonad d ()
dispatchOnExit squad st = case matchState st of
  Just (SomeFS st) -> fsOnExit squad st
  Nothing -> error "Unknown SquadState in dispatchStep"

data SomeFS where
  SomeFS :: (SquadFS st) => st -> SomeFS

matchState :: SquadState -> Maybe SomeFS
matchState (SquadIdleState st)    = Just (SomeFS st)
matchState (SquadFormingState st) = Just (SomeFS st)
matchState (SquadExploreState st) = Just (SomeFS st)


processSquad ::(HasArmy d, AgentDynamicState d) => Squad -> StepMonad d Squad
processSquad squad = do
      (done, state') <- dispatchUpdate squad (squadState squad)
      if done
        then squadTransitionFrom squad state'
        else do
          dispatchStep squad state'
          return squad { squadState = state' }

squadTransitionFrom :: (HasArmy d, AgentDynamicState d) => Squad -> SquadState -> StepMonad d Squad
squadTransitionFrom squad oldState = do
  dispatchOnExit squad oldState

  -- Pick new state (e.g., Idle)
  let stNew = wrapState FSSquadIdle  -- or whatever your decision logic is
  dispatchOnEnter squad stNew

  return squad { squadState = stNew }

data FSSquadIdle = FSSquadIdle

instance SquadFS FSSquadIdle where
    -- type Owner FSSquadIdle = Squad
    wrapState = SquadIdleState
    unwrapState (SquadIdleState st) = Just st
    unwrapState _ = Nothing

    fsStep s _ = traceM "[step] idle" -- >> wanderAround 5
    fsUpdate _ st = pure (True, st)
    fsOnEnter s _ = traceM $ "[enter] Idle " ++ show (squadId s)
    fsOnExit  s _ = traceM $ "[exit] Idle " ++ show (squadId s)

type SquadFormation = (TilePos, Footprint)
newtype FSSquadForming = FSSquadForming (Maybe SquadFormation)

instance SquadFS FSSquadForming where
    -- type Owner FSSquadForming = Squad
    wrapState = SquadFormingState
    unwrapState (SquadFormingState st) = Just st
    unwrapState _ = Nothing

    fsStep s (FSSquadForming Nothing) = pure ()
    fsStep s (FSSquadForming (Just (fcenter, formation))) = do
         traceM "[step] idle"
         void $ squadMoveToFormation s fcenter formation
    fsUpdate s st@(FSSquadForming Nothing) = do -- --TODO: we can try find formation
        ds <- agentGet
        let formation = squadFormationFootprint
            unitByTag t = HashMap.lookup t (getUnitMap ds)
            -- TODO: it shouldn't happen: updateArmy had to remove dead units from squads
            units = catMaybes $ [unitByTag t | t <- squadUnits s]
            leader = head units
            leaderTpos = tilePos $ leader ^. #pos
        -- TODO: magic number: consider implement findplacementinregion
        gatherPlace <- findPlacementPointInRadiusSM formation leaderTpos 10
        case gatherPlace of
            Nothing -> do
                isFull <- isSquadFull s
                return (isFull, st)
            (Just fcenter) -> addMarkSM formation fcenter >> return (False, FSSquadForming (Just(fcenter, formation)))

    fsUpdate s st@(FSSquadForming(Just (center, formation))) = do
        isFull <- isSquadFull s
        isFormed <- isSquadFormed s center formation
        return (isFull && isFormed, st)

    fsOnEnter squad (FSSquadForming Nothing) = do
        traceM $ "[enter] FSSquadForming " ++ show (squadId squad)
    fsOnEnter s _ = pure ()
    fsOnExit s (FSSquadForming f)  = do
        traceM $ "[exit] FSSquadForming " ++ show (squadId s)
        case f of
            Nothing -> pure ()
            Just (center, fprint) -> void $ removeMarkSM fprint center


data FSExploreRegion = FSExploreRegion RegionId Region

instance SquadFS FSExploreRegion where
    -- type Owner FSExploreRegion = Squad
    wrapState = SquadExploreState
    unwrapState (SquadExploreState st) = Just st
    unwrapState _ = Nothing

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