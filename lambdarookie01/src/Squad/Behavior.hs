{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Squad.Behavior where

import Actions (Action (..), UnitTag)

import Army.Class
import Footprint
import SC2.Geometry
import SC2.Grid.Algo
import SC2.TilePos
import SC2.Ids.AbilityId
import SC2.Spatial
import SC2.Utils
import Squad.Squad
import StepMonad
import Target (Target)

import Army.Army (armyByTag)
import Control.Monad (filterM, void, when)
import Data.Char (isDigit)
import Data.HashMap.Strict qualified as HashMap
import Data.Maybe
import Data.Set qualified as Set
import Debug.Trace
import Lens.Micro
import Lens.Micro.Extras (view)
import System.Random (Random, StdGen, randomR)

isSquadFull :: (HasArmy d) => FSMSquad a -> StepMonad d Bool
isSquadFull squad = do
  ds <- agentGet
  let unitMap = getUnitMap ds
      tags = squadTags squad
      -- TODO: magic number
      squadSize = 5
  return $
    length tags == squadSize
      && all (`HashMap.member` unitMap) tags
      && all (\t -> let Just u = HashMap.lookup t unitMap in (1.0 :: Float) == u ^. #buildProgress) tags

-- wanderAround :: FS s => s Int -> StepMonad d s
wanderAround s radius = pure ()

-- command to move units to formation. returns true when complete
squadMoveToFormation ::
  (HasArmy d, HasObs d) => FSMSquad a -> TilePos -> Footprint -> StepMonad d Bool
squadMoveToFormation squad center@(cx, cy) (Footprint formation) = do
  ds <- agentGet
  let unitByTag t = HashMap.lookup t (getUnitMap ds)
      -- TODO: it shouldn't happen: updateArmy had to remove dead units from squads
      (leader : units) = catMaybes $ [unitByTag t | t <- squadTags squad]
      -- filter out leader 'c' : leader goes to center
      unitsFormationPos = (\(dx, dy, _) -> center + (dx, dy)) <$> filter (\(_, _, ch) -> isDigit ch) formation

      unitsWithPos = take (length units) unitsFormationPos `zip` units

  -- if all (\(p, u) -> p == (tilePos . view #pos $ u) ) unitsWithPos
  if all (\(p, u) -> 2 >= distManhattan p (tilePos . view #pos $ u)) unitsWithPos
    then return True
    else do
      command [PointCommand ATTACKATTACK [leader] (toPoint2D center)]
      command [PointCommand ATTACKATTACK [u] (toPoint2D p) | (p, u) <- unitsWithPos]
      return False

squadExploreRegion :: (HasArmy d, HasGrid d, HasObs d) => FSMSquad a -> Region -> StepMonad d ()
squadExploreRegion s region =
  do
    grid <- agentGrid
    let targetPos = head $ Set.toList region
        unitTags@(squadLeaderTag : _) = squadTags s
    leaderPos <- tilePos . view #pos . fromJust <$> armyByTag squadLeaderTag

    let GridBfsRes isFound _ path = gridBfs grid leaderPos (getAllNotSharpNeighbors grid) (== targetPos) (const False)
        posToGo = fromJust $ backoffList path 3

    if isNothing isFound
      then void $ traceM ("[warn] squadExploreRegion: unreacheble: " ++ show targetPos)
      else do
        units <- catMaybes <$> mapM armyByTag unitTags
        command [PointCommand ATTACKATTACK units (toPoint2D posToGo)]

squadDoAttack :: FSMSquad a -> Target -> StepMonad d ()
squadDoAttack squad target = return ()

isSquadFormed :: (HasArmy d) => FSMSquad a -> TilePos -> Footprint -> StepMonad d Bool
isSquadFormed squad center formation = do
  squadTags <- catMaybes <$> mapM armyByTag (squadTags squad)
  let -- filter out leader 'c' : leader goes to center
      unitsFormationPos = (\(dx, dy, _) -> center + (dx, dy)) <$> filter (\(_, _, ch) -> isDigit ch) (pixels formation)
      -- TODO: it shouldn't happen: updateArmy had to remove dead units from squads
      (_ : units) = squadTags

      unitsWithPos = take (length units) unitsFormationPos `zip` units
  return $ all (\(p, u) -> 2 >= distManhattan p (tilePos . view #pos $ u)) unitsWithPos
