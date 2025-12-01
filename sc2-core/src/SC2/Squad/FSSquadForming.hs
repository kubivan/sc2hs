
module SC2.Squad.FSSquadForming where

import SC2.Squad.Class

import SC2.Squad.Squad
import SC2.Squad.Behavior
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

type SquadFormation = (TilePos, Footprint)
newtype FSSquadForming = FSSquadForming (Maybe SquadFormation)

instance SquadFS FSSquadForming where

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