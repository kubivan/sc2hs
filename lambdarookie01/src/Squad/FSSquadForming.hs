
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

formingStep :: (HasArmy d, HasObs d, HasGrid d) => FSMSquad SquadState -> Maybe SquadFormation -> StepMonad d ()
formingStep s Nothing = pure ()
formingStep s (Just (fcenter, formation)) = do
    traceM "[step] forming"
    void $ squadMoveToFormation s fcenter formation

-- ---------------------------------------------------------------------------
-- Update

formingUpdate :: (HasArmy d, HasObs d, HasGrid d) => FSMSquad SquadState -> Maybe SquadFormation -> StepMonad d (Bool, SquadState)
formingUpdate s Nothing = do
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
            return (isFull, SSForming Nothing)
        (Just fcenter) -> addMarkSM formation fcenter >> return (False, SSForming (Just (fcenter, formation)))

formingUpdate s (Just (center, formation)) = do
    isFull <- isSquadFull s
    isFormed <- isSquadFormed s center formation
    return (isFull && isFormed, SSForming (Just (center, formation)))

-- ---------------------------------------------------------------------------
-- Enter / Exit / Transition

formingOnEnter :: (HasArmy d) => FSMSquad SquadState -> Maybe SquadFormation -> StepMonad d ()
formingOnEnter squad Nothing = traceM $ "[enter] FSSquadForming " ++ show (squadId squad)
formingOnEnter _ _ = pure ()

formingOnExit :: (HasArmy d, HasGrid d) => FSMSquad SquadState -> Maybe SquadFormation -> StepMonad d ()
formingOnExit s f = do
    traceM $ "[exit] FSSquadForming " ++ show (squadId s)
    case f of
        Nothing -> pure ()
        Just (center, fprint) -> void $ removeMarkSM fprint center

formingTransitionNext :: (HasArmy d) => FSMSquad SquadState -> StepMonad d SquadState
formingTransitionNext _ = pure SSIdle
