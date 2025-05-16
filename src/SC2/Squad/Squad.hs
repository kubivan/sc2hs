{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs, ExistentialQuantification, RankNTypes #-}
{-# LANGUAGE GADTs, ConstraintKinds, TypeApplications #-}

module SC2.Squad.Squad where

import Actions (Action (..), UnitTag)
import SC2.Grid.Algo
import SC2.Grid.TilePos
import SC2.Squad.Class
import SC2.Squad.Types
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

isSquadFull :: (HasArmy d) => Squad a -> StepMonad d Bool
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

-- replaceSquad :: Squad -> [Squad] -> [Squad]
replaceSquad new = map (\s -> if squadId s == squadId new then new else s)

--wanderAround :: FS s => s Int -> StepMonad d s
wanderAround s radius = pure ()

-- command to move units to formation. returns true when complete
squadMoveToFormation :: (HasArmy d, AgentDynamicState d) => Squad a -> TilePos -> Footprint -> StepMonad d Bool
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

squadExploreRegion :: (HasArmy d, AgentDynamicState d) => Squad a -> Region -> StepMonad d ()
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

squadDoAttack :: Squad a -> Target -> StepMonad d ()
squadDoAttack squad target = return ()

isSquadFormed :: (HasArmy d, AgentDynamicState d) => Squad a -> TilePos -> Footprint -> StepMonad d Bool
isSquadFormed squad center formation = do
        ds <- agentGet
        let unitByTag t = HashMap.lookup t (getUnitMap ds)
            -- TODO: it shouldn't happen: updateArmy had to remove dead units from squads
            (leader : units) = catMaybes $ [unitByTag t | t <- squadUnits squad]
            -- filter out leader 'c' : leader goes to center
            unitsFormationPos = (\(dx, dy, _) -> center + (dx, dy)) <$> filter (\(_, _, ch) -> isDigit ch) (pixels formation)

            unitsWithPos = take (length units) unitsFormationPos `zip` units
        return $ all (\(p, u) -> 2 >= distManhattan p (tilePos . view #pos $ u)) unitsWithPos