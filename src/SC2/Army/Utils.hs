module SC2.Army.Utils where

import Actions (Action (..), UnitTag)
import Footprint
import Observation
import SC2.Army.Army
import SC2.Army.Class
import SC2.Geometry
import SC2.Grid
import SC2.Ids.AbilityId
import SC2.Ids.UnitTypeId
import SC2.Proto.Data (Alliance (..), Point, Point2D)
import SC2.Proto.Data qualified as Proto
import StepMonad
import Units
import Utils

import Conduit (filterC, mapC)
import Control.Applicative ((<|>))
import Control.Monad (filterM, void, when)
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

squadFormationFootprint :: Footprint
squadFormationFootprint = createFootprint $ unlines ["1#2#c#3#4"]

isSquadFull :: (HasArmy d) => ArmySquad -> StepMonad d Bool
isSquadFull squad = do
    ds <- agentGet
    let unitMap = armyUnits $ getArmy ds
        tags = squadUnits squad
        -- TODO: magic number
        squadSize = 5
    return $ length tags == squadSize && all (`HashMap.member` unitMap) tags && all (\t -> let Just u = HashMap.lookup t unitMap in (1.0 :: Float) == u ^. #buildProgress) tags

isArmyUnit :: Unit -> Bool -- TODO: remove protoss specific consts
isArmyUnit u = ProtossProbe /= utype && (not . isBuildingType $ utype)
  where
    utype = toEnum' (u ^. #unitType)

-- Define an enemy unit filter
isEnemy :: Unit -> Bool
isEnemy u = (u ^. #alliance) == Enemy

-- Get neighboring tiles
neighbors :: TilePos -> Grid -> [TilePos]
neighbors p@(x, y) grid =
    [ (x + dx, y + dy)
    | dx <- [-1, 0, 1]
    , dy <- [-1, 0, 1]
    , dx /= 0 || dy /= 0 -- Exclude points on the same vertical line
    , let pixel = grid !? (x + dx, y + dy)
    , pixel /= Just '#' && isJust pixel
    ]

tilesInRadius :: Int -> TilePos -> [TilePos]
tilesInRadius r (x, y) =
    [ (x + dx, y + dy)
    | dx <- [-r .. r]
    , dy <- [-r .. r]
    , dx * dx + dy * dy <= r * r -- circular mask
    -- , (dx, dy) /= (0, 0)          -- exclude center
    ]

backoffList :: [a] -> Int -> Maybe a
backoffList xs n
    | n < 0 = Nothing
    | otherwise = takeMaybe n xs <|> backoffList xs (n - 1)
  where
    takeMaybe i ys = if i < length ys then Just (ys !! i) else Nothing

-- Check if an enemy is in range
enemyInRange :: Unit -> [Unit] -> Maybe Unit
enemyInRange u enemies =
    headMay $ filter (\e -> distSquared (e ^. #pos) (u ^. #pos) <= 6 * 6) enemies -- TODO: magic number Stalker attack range of 6

noneOf :: (a -> Bool) -> [a] -> Bool
noneOf p = not . any p

-- Get the direction vector from one position to another
directionTo :: Point2D -> Point2D -> (Int, Int)
directionTo p1 p2 = (round $ signum (p2 ^. #x - p1 ^. #x), round $ signum (p2 ^. #y - p1 ^. #y))

-- Get the nearest enemy unit
nearestEnemy :: Unit -> [Unit] -> Maybe Unit
nearestEnemy u = minimumByMay (comparing (distSquared (u ^. #pos) . (^. #pos)))

nearestTarget :: Unit -> [Point2D] -> Maybe Point2D
nearestTarget u = minimumByMay (comparing (distSquared (toPoint2D $ u ^. #pos)))

selfBuildingsCount :: Observation -> Int
selfBuildingsCount obs = length . runC $ unitsSelf obs .| filterC isBuilding