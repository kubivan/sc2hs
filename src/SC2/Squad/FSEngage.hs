
module SC2.Squad.FSEngage where

import Actions
import Units
import SC2.Grid
import SC2.Squad.Class
import SC2.Squad.Squad
import SC2.Geometry
import StepMonad
import StepMonadUtils
import SC2.Ids.AbilityId
import SC2.Ids.UnitTypeId
import Observation

import Data.HashMap.Strict qualified as HashMap
import Data.Maybe
import Control.Monad

import Lens.Micro
import Lens.Micro.Extras (view)

import Debug.Trace

import Proto.S2clientprotocol.Common ( Point2D )

data FSEngage = FSEngageFar UnitTag | FSEngageClose UnitTag


isWeaponReady :: Unit -> Bool
isWeaponReady u = (u ^. #weaponCooldown) == 0

isEnemyInRange :: AgentDynamicState d => Unit -> Unit -> StepMonad d Bool
isEnemyInRange e u = do
    range <- siUnitRange u e
    return $ distSquared (u ^. #pos) (e ^. #pos) <= range * range

canAttack :: AgentDynamicState d => Unit -> Unit -> StepMonad d Bool
canAttack u e = do
    inRange <- isEnemyInRange e u
    return $ isWeaponReady u && inRange

-- when (null . view #orders $ u) $
stepForwardOrBack ::AgentDynamicState d =>  Unit -> Unit -> StepMonad d ()
stepForwardOrBack u e = do
    range <- siUnitRange u e
    let distSq = distSquared (u ^. #pos) (e ^. #pos)
        upos = toPoint2D $ u ^. #pos
    if distSq / 2 >= (range / 2) * (range / 2) then do
        stepForward <- u `stepToward` e
        command [PointCommand MOVE [u] (upos + stepForward)] -- TODO: 1 stepForward vec
    else do
        stepBack <- stepBackward u e
        command [PointCommand MOVE [u] (upos + stepBack) ]

fleeVelocity :: Point2D -> Point2D -> Float -> Point2D
fleeVelocity currentPos targetPos speed =
  vecNormalize $ vecScale speed (currentPos - targetPos)

seekVelocity :: Point2D -> Point2D -> Float -> Point2D
seekVelocity currentPos targetPos speed =
  vecNormalize $ vecScale speed (targetPos - currentPos)

stepBackward :: AgentDynamicState d => Unit -> Unit -> StepMonad d Point2D
stepBackward u e = do
    udata <- siUnitData u
    let uspeed = udata ^. #movementSpeed
        enemyPos2D = toPoint2D $ e ^. #pos
        unitPos2D = toPoint2D $ u ^. #pos
    return $ fleeVelocity unitPos2D enemyPos2D uspeed

stepToward :: AgentDynamicState d => Unit -> Unit -> StepMonad d Point2D
stepToward u e = do
    udata <- siUnitData u
    let uspeed = udata ^. #movementSpeed
        enemyPos2D = toPoint2D $ e ^. #pos
        unitPos2D = toPoint2D $ u ^. #pos
    return $ seekVelocity unitPos2D enemyPos2D uspeed

unitEngageBehaviorTree :: AgentDynamicState d => Unit -> Unit -> StepMonad d ()
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
    in trace (show (unit ^. #tag) ++ " egaging: " ++ enemyStr enemy ++ ": " ++ show (desiredVelocity - unitVelocity))
        desiredVelocity - unitVelocity


instance SquadFS FSEngage where

    fsStep squad (FSEngageFar enemyTag) = do
        ds <- agentGet
        obs <- agentObs
        let unitByTag t = HashMap.lookup t (getUnitMap ds)
            enemy = fromJust $ getUnit obs enemyTag
            -- TODO: it shouldn't happen: updateArmy had to remove dead units from squads
            units = catMaybes $ [unitByTag t | t <- squadUnits squad]
        traceM ("[step] FSEngageFar " ++ show (squadId squad))
        command [UnitCommand ATTACKATTACK units enemy]

    fsStep squad (FSEngageClose enemyTag) = do
        ds <- agentGet
        obs <- agentObs
        traceM ("[step] FSSeek " ++ show (squadId squad))
        let unitByTag t = HashMap.lookup t (getUnitMap ds)
            enemy = fromJust $ getUnit obs enemyTag
            -- TODO: it shouldn't happen: updateArmy had to remove dead units from squads
            units = catMaybes $ [unitByTag t | t <- squadUnits squad]

        debugUnit enemy
        mapM_ (\u -> debugUnitVec u (unitSeek u enemy)) units
        mapM_ (`unitEngageBehaviorTree` enemy) units


    fsUpdate squad st@(FSEngageClose enemyTag) =
        do
            ds <- agentGet
            obs <- agentObs
            case getUnit obs enemyTag of
                Nothing -> do
                    traceM ("no more unit with tag " ++ show enemyTag)
                    return (True, st)

                _ -> return (False, FSEngageClose enemyTag)

    fsUpdate squad st@(FSEngageFar enemyTag) =
        do
            ds <- agentGet
            obs <- agentObs
            let unitByTag t = HashMap.lookup t (getUnitMap ds)
                enemy = getUnit obs enemyTag
                leader = fromJust $ unitByTag . head . squadUnits $ squad
            --TODO: need more fine control, squad leader may not be able to focus target
            inRange <- isEnemyInRange (fromJust enemy) leader

            case enemy of
                Nothing -> do
                    traceM ("no more unit with tag " ++ show enemyTag)
                    return (True, st)

                _ -> return (False, if inRange then FSEngageClose enemyTag else st)

    fsOnEnter s _ = traceM $ "[enter] FSEngage " ++ show (squadId s)
    fsOnExit  s _ = traceM $ "[exit] FSEngage " ++ show (squadId s)
