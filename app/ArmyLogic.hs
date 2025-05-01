{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLabels #-}

module ArmyLogic where

import Agent
import BotDynamicState

import AbilityId
import Actions (Action (..), UnitTag)
import Grid.Grid
import Observation
import UnitTypeId
import Units
import Utils

import Data.HashMap.Strict qualified as HashMap
import Data.List (minimumBy, partition)
import Data.List.Split (chunksOf)
import Data.Sequence qualified as Seq
import Data.Set qualified as Set

import Control.Monad(when, void)

import Conduit (filterC, findC, headC, lengthC, mapC, runConduitPure, (.|))
import Data.Foldable qualified as Seq
import Data.Function (on)
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Ord (comparing)
import Lens.Micro (to, (&), (.~), (^.), (^..))
import Lens.Micro.Extras (view)
import Safe (headMay, minimumByMay)
import System.Random (Random, StdGen, newStdGen, randomR)

import Proto.S2clientprotocol.Common as C
import Proto.S2clientprotocol.Raw qualified as R
import Debug.Trace (traceM)

-- Define if a unit is considered an army unit
isArmyUnit :: Unit -> Bool -- TODO: remove protoss specific consts
isArmyUnit u = ProtossProbe /= utype && (not . isBuildingType $ utype)
  where
    utype = toEnum' (u ^. #unitType)

-- Define an enemy unit filter
isEnemy :: Unit -> Bool
isEnemy u = (u ^. #alliance) == R.Enemy -- Enemy alliance code

agentUpdateArmyPositions :: StepMonad BotDynamicState ()
agentUpdateArmyPositions = do
    ds <- agentGet
    obs <- agentObs -- allUnitPos = Set.fromList $ runC $ obsUnitsC obs .| filterC (not.isBuilding) .| mapC (view #pos) .| mapC tilePos
    let armyPoss = runC $ unitsSelf obs .| filterC isArmyUnit .| mapC (tilePos . view #pos)
        army' = (dsArmy ds){armyUnitsPos = Set.fromList armyPoss}

    agentPut $ ds{dsArmy = army'}

noneOf :: (a -> Bool) -> [a] -> Bool
noneOf p = not . any p

debugTraceM :: Bool -> String -> StepMonad BotDynamicState ()
debugTraceM cond msg = when cond $ Control.Monad.void (traceM msg)


agentUpdateDsArmy :: StepMonad BotDynamicState ()
agentUpdateDsArmy = do
    ds <- agentGet
    obs <- agentObs -- allUnitPos = Set.fromList $ runC $ obsUnitsC obs .| filterC (not.isBuilding) .| mapC (view #pos) .| mapC tilePos
    let obsArmyUnits = unitsSelf obs .| filterC isArmyUnit
        obsArmyUnitsPoss = runC $ obsArmyUnits .| mapC (tilePos . view #pos)
        armyHashMap = HashMap.fromList $ [( u ^. #tag :: UnitTag, u) | u <- runC obsArmyUnits ]
        squadSize = 5

        fillSquads :: [ArmySquad] -> [UnitTag] -> ([ArmySquad], [UnitTag])
        fillSquads [] rest = ([], rest)
        fillSquads squads [] = (squads, [])
        fillSquads (s:rest) rookies =
            let aliveUnits = filter (`HashMap.member` armyHashMap) (squadUnits s)
                unitsNeeded = squadSize - length aliveUnits
                (assigned, remaining) = splitAt unitsNeeded rookies
                updatedSquad = s { squadUnits = aliveUnits ++ assigned }
                (filledRest, leftover) = fillSquads rest remaining
            in (updatedSquad : filledRest, leftover)

        isSquadFull :: ArmySquad -> Bool
        isSquadFull squad = (all (`HashMap.member` armyHashMap) (squadUnits squad)) && (squadSize == length (squadUnits squad))

        isSquadEmpty :: ArmySquad -> Bool
        isSquadEmpty squad = noneOf (`HashMap.member` armyHashMap) (squadUnits squad)

        (squadsFull, squadsToCheck) = partition isSquadFull (armySquads (dsArmy ds))
        (squadsDead, squadsNotFull) = partition isSquadEmpty squadsToCheck


        squadedUnitTags = Set.fromList $ foldl' (\acc squad -> acc ++ squadUnits squad) [] (squadsFull ++ squadsNotFull)

        freeArmyUnitTags = runC $ obsArmyUnits .| mapC (view #tag) .| filterC (\utag -> not $ utag `Set.member` squadedUnitTags)
        (refilledSquads, restUnits) = fillSquads squadsNotFull freeArmyUnitTags

        newSquadUnits = chunksOf 5 restUnits
        newSquads = concatMap (\us -> [ArmySquad{squadUnits = us, squadState = Idle}]) newSquadUnits

        army' = (dsArmy ds){armyUnitsPos = Set.fromList obsArmyUnitsPoss, armyUnits = armyHashMap, armySquads = squadsFull ++ refilledSquads ++ newSquads}

    _ <- debugTraceM (not . null $ squadsDead) ("squad is dead: " ++ show squadsDead)
    _ <- debugTraceM (not . null $ newSquads) ("squads are formed: " ++ show newSquads)
    _ <- debugTraceM (refilledSquads /= squadsNotFull && (not . null $ refilledSquads)) ("squads are refilled: " ++ show refilledSquads)
    agentPut $ ds{dsArmy = army'}

-- Update the visited tiles for a unit in the army
updateVisitedTile :: UnitTag -> TilePos -> StepMonad BotDynamicState ()
updateVisitedTile tag tile = do
    ds <- agentGet
    let grid = getGrid ds
        army = armyUnitsData . dsArmy $ ds
        defaultUnitData = ArmyUnitData Set.empty (Set.fromList . Seq.toList $ neighbors tile grid)
        unitData = HashMap.lookupDefault defaultUnitData tag army
        newVisited = Set.insert tile (auVisitedTiles unitData)
        newUnvisitedEdge =
            Set.foldl'
                ( \accEdge vt ->
                    let unvisitedNs = Set.fromList [n | n <- Seq.toList (neighbors vt grid), n `Set.notMember` newVisited]
                     in accEdge `Set.union` unvisitedNs
                )
                Set.empty
                (auUnvisitedEdge unitData)

        newUnitData = unitData{auVisitedTiles = newVisited, auUnvisitedEdge = newUnvisitedEdge}

    agentPut $ bdsUpdateArmyUnitData ds tag newUnitData -- ds { dsArmy = {Army (HashMap.insert tag newUnitData army)} }

randomArmyFiddling :: StepMonad BotDynamicState ()
randomArmyFiddling = do
    obs <- agentObs
    ds <- agentGet
    -- Retrieve army units and enemies
    let armyUs = runC $ unitsSelf obs .| filterC isArmyUnit
        grid = getGrid ds -- Retrieve the grid from the dynamic state

    -- Execute a random command for each unit in the army
    mapM_
        ( \u ->
            let unitData = HashMap.lookupDefault (ArmyUnitData Set.empty Set.empty) (u ^. #tag) (armyUnitsData . dsArmy $ ds)
             in randCmd2 grid unitData u
        )
        armyUs

randCmd2 :: Grid -> ArmyUnitData -> Unit -> StepMonad BotDynamicState ()
randCmd2 grid udata u = do
    obs <- agentObs
    let enemies = runC $ obsUnitsC obs .| filterC isEnemy
    -- Check if there's an enemy in range
    case enemyInRange u enemies of
        Just e -> command [Actions.UnitCommand Attack u e] -- Attack the enemy
        Nothing ->
            if not . null . view #orders $ u
                then return ()
                else do
                    -- Move to a random neighboring position if no enemy is in range
                    si <- agentStatic
                    ds <- agentGet
                    let army = dsArmy ds
                        allUnitPos = armyUnitsPos army
                        unvisitedNeighbors = filter (`Set.notMember` allUnitPos) $ filter (\p -> p `Set.notMember` auVisitedTiles udata) (neighbors upos grid)

                    pos <- calcMovePos unvisitedNeighbors

                    command [PointCommand Move u (toPoint2D pos)] -- Move to the random position
  where
    upos = tilePos (u ^. #pos)
    nearest :: (Pointable p) => p -> [p] -> p
    nearest p = minimumBy (compare `on` distSquared p)

    calcMovePos [] = return $ nearest upos (Set.toList (auUnvisitedEdge udata))
    calcMovePos candidates = do
        ds <- agentGet
        rnd <- randGen <$> agentGet -- Retrieve the current random generator
        let scored = scoreMoveCandidates upos udata candidates
            (wrandPos, rnd') = weightedRandomChoice scored rnd -- `Utils.dbg` ("scored: " ++ show scored)
        agentPut $ setRandGen rnd' ds -- Update the random generator in the dynamic state
        updateVisitedTile (u ^. #tag) wrandPos
        return wrandPos

scoreMoveCandidates :: TilePos -> ArmyUnitData -> [TilePos] -> [(TilePos, Double)]
scoreMoveCandidates upos udata = map (\tile -> (tile, calcScore tile))
  where
    calcScore tile = baseScore + unvisitedScore tile -- + closeToUnvisitedEdgeScore tile -- + enemyScore tile
    baseScore = 0.0
    unvisitedScore tile
        | tile `Set.member` auVisitedTiles udata = 1.0
        | otherwise = 10.0

    closeToUnvisitedEdgeScore tile =
        Set.foldl'
            ( \score edgeTile ->
                let distToEdge :: TilePos -> Float
                    distToEdge = distSquared (edgeTile :: TilePos)
                    isCloserToEdgeThen :: TilePos -> TilePos -> Bool
                    a `isCloserToEdgeThen` b = distToEdge a < distToEdge b
                 in if tile `isCloserToEdgeThen` upos then score + 5.0 else score
            )
            0
            (auUnvisitedEdge udata)

weightedRandomChoice :: [(a, Double)] -> StdGen -> (a, StdGen)
weightedRandomChoice weightedItems gen = (selectItem weightedItems r, newGen)
  where
    totalWeight = sum (map snd weightedItems) -- Sum of all weights
    (r, newGen) = randomR (0, totalWeight) gen -- Generate a random number between 0 and the total weight

selectItem :: [(a, Double)] -> Double -> a
selectItem ((item, weight) : xs) r
    | r <= weight = item -- If random value is within current item's weight, return it
    | otherwise = selectItem xs (r - weight) -- Otherwise, subtract the weight and move to the next item
selectItem [] _ = error "weightedRandomChoice: empty list"

-- Get the direction vector from one position to another
directionTo :: Point2D -> Point2D -> (Int, Int)
directionTo p1 p2 = (round $ signum (p2 ^. #x - p1 ^. #x), round $ signum (p2 ^. #y - p1 ^. #y))

-- Get the nearest enemy unit
nearestEnemy :: Unit -> [Unit] -> Maybe Unit
nearestEnemy u = minimumByMay (comparing (distSquared (u ^. #pos) . (^. #pos)))

nearestTarget :: Unit -> [Point2D] -> Maybe Point2D
nearestTarget u = minimumByMay (comparing (distSquared (toPoint2D $ u ^. #pos)))

-- Function to select a random element from a list of weights
weightedRandom :: [Double] -> StdGen -> (Int, StdGen)
weightedRandom weights gen =
    let totalWeight = sum weights
        (randValue, newGen) = randomR (0, totalWeight) gen -- Random value between 0 and the total weight
     in (selectIndex weights randValue, newGen)

-- Function to select the index based on the random value and the cumulative sum of weights
selectIndex :: [Double] -> Double -> Int
selectIndex weights randValue = go weights randValue 0
  where
    go (w : ws) rv idx
        | rv <= w = idx -- If random value is less than or equal to current weight, return the index
        | otherwise = go ws (rv - w) (idx + 1) -- Subtract the weight and continue
    go [] _ idx = idx -- Default case, in case something goes wrong (should not happen with proper weights)

-- Get neighboring tiles
neighbors :: TilePos -> Grid -> [TilePos]
neighbors p@(x, y) grid =
    [ (x + dx, y + dy)
    | dx <- [-1, 0, 1]
    , dy <- [-1, 0, 1]
    , dx /= 0 || dy /= 0 -- Exclude points on the same vertical line
    , let pixel = gridPixelSafe grid (x + dx, y + dy)
    , pixel /= Just '#' && isJust pixel
    ]

-- Check if an enemy is in range
enemyInRange :: Unit -> [Unit] -> Maybe Unit
enemyInRange u enemies =
    headMay $ filter (\e -> distSquared (e ^. #pos) (u ^. #pos) <= 6 * 6) enemies -- Stalker attack range of 6

agentUpdateArmy :: Observation -> StepMonad BotDynamicState ()
agentUpdateArmy obsPrev = agentUpdateDsArmy >> agentUpdateArmyPositions -- TODO: no diff with obsPrev

selfBuildingsCount :: Observation -> Int
selfBuildingsCount obs = length . runC $ unitsSelf obs .| filterC isBuilding