
module Squad.FSSquadForming where

import Squad.Class

import Squad.Squad
import Squad.State
import Squad.Behavior
import SC2.Utils
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

-- ---------------------------------------------------------------------------
-- Step

formingStep :: (HasArmy d, HasObs d, HasGrid d) => FSMSquad SquadState -> FSForming -> StepMonad d ()
formingStep s FSFormingUnplaced = pure ()
formingStep s (FSFormingPlaced (fcenter, formation)) = do
    traceM "[step] forming"
    void $ squadMoveToFormation s fcenter formation

-- ---------------------------------------------------------------------------
-- Update

formingUpdate :: (HasArmy d, HasObs d, HasGrid d) => FSMSquad SquadState -> FSForming -> StepMonad d UpdateResult
formingUpdate s FSFormingUnplaced = do
    ds <- agentGet
    let formation = squadFormationFootprint
        unitByTag t = HashMap.lookup t (getUnitMap ds)
        units = catMaybes $ [unitByTag t | t <- squadUnits s]
        leader = head units
        leaderTpos = tilePos $ leader ^. #pos
    gatherPlace <- findPlacementPointInRadiusSM formation leaderTpos 10
    case gatherPlace of
        Nothing -> do
            isFull <- isSquadFull s
            return $ if isFull
                then Transition SSIdle
                else Continue (SSForming FSFormingUnplaced)
        (Just fcenter) -> do
            addMarkSM formation fcenter
            return $ Continue (SSForming (FSFormingPlaced (fcenter, formation)))

formingUpdate s (FSFormingPlaced (center, formation)) = do
    isFull <- isSquadFull s
    isFormed <- isSquadFormed s center formation
    return $ if isFull && isFormed
        then Transition SSIdle
        else Continue (SSForming (FSFormingPlaced (center, formation)))

-- ---------------------------------------------------------------------------
-- Enter / Exit / Transition

formingOnEnter :: (HasArmy d) => FSMSquad SquadState -> FSForming -> StepMonad d ()
formingOnEnter squad FSFormingUnplaced = traceM $ "[enter] FSSquadForming " ++ show (squadId squad)
formingOnEnter _ _ = pure ()

formingOnExit :: (HasArmy d, HasGrid d) => FSMSquad SquadState -> FSForming -> StepMonad d ()
formingOnExit s FSFormingUnplaced = traceM $ "[exit] FSSquadForming " ++ show (squadId s)
formingOnExit s (FSFormingPlaced (center, fprint)) = do
    traceM $ "[exit] FSSquadForming " ++ show (squadId s)
    void $ removeMarkSM fprint center
