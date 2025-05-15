{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs, ExistentialQuantification, RankNTypes #-}

module SC2.Army.SquadFSM where

import SC2.Army.Behavior
import Actions (Action (..), UnitTag)
import Observation
-- import SC2.Army.Army
import SC2.Army.Utils
import SC2.Army.Class
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

import Data.Typeable

isSquadIdle :: Squad -> Bool
isSquadIdle s = case squadState s of
  AnyFS state ->
    case cast state :: Maybe FSSquadIdle of
      Just _  -> True
      Nothing -> False

squadAssignedRegion :: Squad -> Maybe RegionId
squadAssignedRegion squad = case squadState squad of
  AnyFS state ->
    case cast state :: Maybe FSExploreRegion of
      Just (FSExploreRegion rid _) -> Just rid
      Nothing -> Nothing


processSquad ::(HasArmy d, AgentDynamicState d) => Squad -> StepMonad d Squad
processSquad squad = do
  case squadState squad of
    anys@(AnyFS state) -> do
      (done, state') <- fsUpdate squad state
      if done
        then squadTransitionFrom squad anys
        else do
          fsStep squad state'
          return squad { squadState = AnyFS state' }

squadTransitionFrom :: (HasArmy d, AgentDynamicState d) => Squad -> AnyFS -> StepMonad d Squad
squadTransitionFrom squad oldState@(AnyFS stOld) = do
  fsOnExit squad stOld  -- uses FS constraint from AnyFS

  -- Pick new state (e.g., Idle)
  let stNew = FSSquadIdle  -- or whatever your decision logic is
  fsOnEnter squad stNew

  return squad { squadState = AnyFS stNew }

data FSSquadIdle = FSSquadIdle
deriving instance Typeable FSSquadIdle

instance SquadFS FSSquadIdle where
    -- type Owner FSSquadIdle = Squad

    fsStep s _ = traceM "[step] idle" -- >> wanderAround 5
    fsUpdate _ st = pure (True, st)
    fsOnEnter s _ = traceM $ "[enter] Idle " ++ show (squadId s)
    fsOnExit  s _ = traceM $ "[exit] Idle " ++ show (squadId s)

type SquadFormation = (TilePos, Footprint)
newtype FSSquadForming = FSSquadForming (Maybe SquadFormation)

instance SquadFS FSSquadForming where
    -- type Owner FSSquadForming = Squad

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