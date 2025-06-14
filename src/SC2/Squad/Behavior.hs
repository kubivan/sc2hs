{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs, ExistentialQuantification, RankNTypes #-}
{-# LANGUAGE GADTs, ConstraintKinds, TypeApplications #-}

module SC2.Squad.Behavior where

import Actions (Action (..), UnitTag)
import SC2.Grid.Algo
import SC2.Grid.TilePos
import SC2.Squad.Squad
import SC2.Squad.Class
import Units
import Utils
import Footprint
import StepMonad
import SC2.Geometry
import SC2.Utils
import SC2.Ids.AbilityId

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Maybe
import System.Random (Random, StdGen, randomR)
import Lens.Micro
import Lens.Micro.Extras (view)
import Data.Char (isDigit)
import Data.Typeable
import Debug.Trace
import Control.Monad (filterM, void, when)

isSquadFull :: (HasArmy d) => FSMSquad a -> StepMonad d Bool
isSquadFull squad = do
    ds <- agentGet
    let unitMap = getUnitMap ds
        tags = squadUnits squad
        -- TODO: magic number
        squadSize = 5
    return $ length tags == squadSize && all (`HashMap.member` unitMap) tags && all (\t -> let Just u = HashMap.lookup t unitMap in (1.0 :: Float) == u ^. #buildProgress) tags

--wanderAround :: FS s => s Int -> StepMonad d s
wanderAround s radius = pure ()

-- command to move units to formation. returns true when complete
squadMoveToFormation :: (HasArmy d, AgentDynamicState d) => FSMSquad a -> TilePos -> Footprint -> StepMonad d Bool
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
            command [PointCommand ATTACKATTACK [leader] (toPoint2D center)]
            command [PointCommand ATTACKATTACK [u] (toPoint2D p) | (p, u) <- unitsWithPos]
            return False

squadExploreRegion :: (HasArmy d, AgentDynamicState d) => FSMSquad a -> Region -> StepMonad d ()
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
                command [PointCommand ATTACKATTACK [unitByTag ut | ut <- unitTags] (toPoint2D posToGo)]

squadDoAttack :: FSMSquad a -> Target -> StepMonad d ()
squadDoAttack squad target = return ()

isSquadFormed :: (HasArmy d, AgentDynamicState d) => FSMSquad a -> TilePos -> Footprint -> StepMonad d Bool
isSquadFormed squad center formation = do
        ds <- agentGet
        let unitByTag t = HashMap.lookup t (getUnitMap ds)
            -- TODO: it shouldn't happen: updateArmy had to remove dead units from squads
            (leader : units) = catMaybes $ [unitByTag t | t <- squadUnits squad]
            -- filter out leader 'c' : leader goes to center
            unitsFormationPos = (\(dx, dy, _) -> center + (dx, dy)) <$> filter (\(_, _, ch) -> isDigit ch) (pixels formation)

            unitsWithPos = take (length units) unitsFormationPos `zip` units
        return $ all (\(p, u) -> 2 >= distManhattan p (tilePos . view #pos $ u)) unitsWithPos