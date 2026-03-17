module Squad.FSEngage where

import Actions
import Observation
import SC2.Geometry
import SC2.Grid
import SC2.Ids.AbilityId
import SC2.Ids.UnitTypeId
import SC2.Spatial qualified as Spatial
import Squad.Behavior (isSquadFull)
import Squad.Class
import Squad.FSMLog
import Squad.Squad
import Squad.State
import StepMonad
import StepMonadUtils
import Units

import Control.Monad
import Data.HashMap.Strict qualified as HashMap
import Data.Maybe

import Lens.Micro
import Lens.Micro.Extras (view)

import Debug.Trace (trace)

import Proto.S2clientprotocol.Common (Point2D)
import Proto.S2clientprotocol.Sc2api_Fields (abilityId)

isWeaponReady :: Unit -> Bool
isWeaponReady u = (u ^. #weaponCooldown) == 0

isEnemyInRange :: Unit -> Unit -> StepMonad d Bool
isEnemyInRange e u = do
    range <- siUnitRange u e
    return $ Spatial.distSquared2D u e <= range * range

canAttack :: Unit -> Unit -> StepMonad d Bool
canAttack u e = do
    inRange <- isEnemyInRange e u
    return $ isWeaponReady u && inRange

unitIsNotMoving :: Unit -> Bool
unitIsNotMoving u = noOrders || notMoving
  where
    noOrders = null . view #orders $ u
    order = view #abilityId . head . view #orders $ u
    notMoving = order /= fromEnum' MOVEMOVE

stepForwardOrBack :: (HasObs d) => Unit -> Unit -> StepMonad d ()
stepForwardOrBack u e = when (unitIsNotMoving u) $ do
    range <- siUnitRange u e
    let distSq = Spatial.distSquared2D u e
        upos = toPoint2D $ u ^. #pos
    if distSq / 2 >= (range / 2) * (range / 2)
        then do
            stepForward <- u `stepToward` e
            command [PointCommand MOVE [u] (upos + stepForward)]
        else do
            stepBack <- stepBackward u e
            command [PointCommand MOVE [u] (upos + stepBack)]

fleeVelocity :: Point2D -> Point2D -> Float -> Point2D
fleeVelocity currentPos targetPos speed =
    vecNormalize $ vecScale speed (currentPos - targetPos)

seekVelocity :: Point2D -> Point2D -> Float -> Point2D
seekVelocity currentPos targetPos speed =
    vecNormalize $ vecScale speed (targetPos - currentPos)

stepBackward :: Unit -> Unit -> StepMonad d Point2D
stepBackward u e = do
    udata <- siUnitData u
    let uspeed = udata ^. #movementSpeed
        enemyPos2D = toPoint2D $ e ^. #pos
        unitPos2D = toPoint2D $ u ^. #pos
    return $ fleeVelocity unitPos2D enemyPos2D uspeed

stepToward :: Unit -> Unit -> StepMonad d Point2D
stepToward u e = do
    udata <- siUnitData u
    let uspeed = udata ^. #movementSpeed
        enemyPos2D = toPoint2D $ e ^. #pos
        unitPos2D = toPoint2D $ u ^. #pos
    return $ seekVelocity unitPos2D enemyPos2D uspeed

unitEngageBehaviorTree :: (HasObs d) => Unit -> Unit -> StepMonad d ()
unitEngageBehaviorTree u e = do
    canAtk <- canAttack u e
    if canAtk
        then
            command [UnitCommand ATTACKATTACK [u] e]
        else
            stepForwardOrBack u e

enemyStr :: Unit -> String
enemyStr u = show (u ^. #tag) ++ ", " ++ show ((toEnum' (u ^. #unitType)) :: UnitTypeId) ++ " " ++ show (tilePos (u ^. #pos))

unitSeek :: Unit -> Unit -> Point2D
unitSeek unit enemy =
    let enemyPos2D = toPoint2D $ enemy ^. #pos
        speed = 4.13 -- TODO: remove hardcoded stalkers value
        desiredVelocity = vecScale speed $ vecNormalize $ enemyPos2D - toPoint2D (unit ^. #pos)
        unitVelocity = unitVelocityVec unit
     in trace
            (show (unit ^. #tag) ++ " egaging: " ++ enemyStr enemy ++ ": " ++ show (desiredVelocity - unitVelocity))
            desiredVelocity
            - unitVelocity

-- ---------------------------------------------------------------------------
-- Step

engageFarStep :: (HasArmy d, HasObs d, HasGrid d) => FSMSquad SquadState -> FSEngage -> StepMonad d ()
engageFarStep squad (FSEngageFar enemyTag) = do
    ds <- agentGet
    obs <- agentObs
    let unitByTag t = HashMap.lookup t (getUnitMap ds)
        enemy = fromJust $ getUnit obs enemyTag
        units = catMaybes $ [unitByTag t | t <- squadUnits squad]
    traceFSM squad "step"
    command [UnitCommand ATTACKATTACK units enemy]
engageFarStep _ _ = pure ()

engageCloseStep :: (HasArmy d, HasObs d, HasGrid d) => FSMSquad SquadState -> FSEngage -> StepMonad d ()
engageCloseStep squad (FSEngageClose enemyTag) = do
    ds <- agentGet
    obs <- agentObs
    traceFSM squad "step"
    let unitByTag t = HashMap.lookup t (getUnitMap ds)
        enemy = fromJust $ getUnit obs enemyTag
        units = catMaybes $ [unitByTag t | t <- squadUnits squad]
    debugUnit enemy
    mapM_ (\u -> debugUnitVec u (unitSeek u enemy)) units
    mapM_ (`unitEngageBehaviorTree` enemy) units
engageCloseStep _ _ = pure ()

-- ---------------------------------------------------------------------------
-- Update

engageCloseUpdate :: (HasArmy d, HasObs d, HasGrid d) => FSMSquad SquadState -> FSEngage -> StepMonad d UpdateResult
engageCloseUpdate squad st@(FSEngageClose enemyTag) = do
    obs <- agentObs
    ds <- agentGet
    let unitByTag t = HashMap.lookup t (getUnitMap ds)
        leader = fromJust $ unitByTag (head (squadUnits squad))
        damaged = leader ^. #shield <= leader ^. #shieldMax / 2
    if damaged
        then do
            traceFSM squad ("leader damaged " ++ show enemyTag)
            return (Transition (SSRetreat Nothing))
        else do
            case getUnit obs enemyTag of
                Nothing -> do
                    traceFSM squad ("no more unit with tag " ++ show enemyTag)
                    full <- isSquadFull squad
                    pure $
                        if full
                            then Transition SSIdle
                            else Transition (SSForming FSFormingUnplaced)
                _ -> pure (Continue (SSEngage st))
engageCloseUpdate _ st = pure (Continue (SSEngage st))

engageFarUpdate :: (HasArmy d, HasObs d, HasGrid d) => FSMSquad SquadState -> FSEngage -> StepMonad d UpdateResult
engageFarUpdate squad st@(FSEngageFar enemyTag) = do
    ds <- agentGet
    obs <- agentObs
    let unitByTag t = HashMap.lookup t (getUnitMap ds)
        enemy = getUnit obs enemyTag
        leader = fromJust $ unitByTag . head . squadUnits $ squad
    inRange <- case enemy of
        Just e -> isEnemyInRange e leader
        Nothing -> return False
    case enemy of
        Nothing -> do
            traceFSM squad ("no more unit with tag " ++ show enemyTag)
            full <- isSquadFull squad
            pure $
                if full
                    then Transition SSIdle
                    else Transition (SSForming FSFormingUnplaced)
        _ -> pure $ Continue (SSEngage (if inRange then FSEngageClose enemyTag else st))
engageFarUpdate _ st = pure (Continue (SSEngage st))

-- ---------------------------------------------------------------------------
-- Enter / Exit / Transition

engageOnEnter :: (HasArmy d) => FSMSquad SquadState -> StepMonad d ()
engageOnEnter squad = traceFSM squad "enter"

engageOnExit :: (HasArmy d) => FSMSquad SquadState -> StepMonad d ()
engageOnExit squad = traceFSM squad "exit"
