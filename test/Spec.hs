{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}

import Conduit
import Grid
import qualified Data.Vector as V
import Footprint
import UnitTypeId

import Control.Arrow ((|||))

import Test.Hspec
import Units
    ( obsUnitsC,
      runC,
      unitTypeC,
      findNexus,
      PointLabel(..),
      MapClusters,
      Unit(..),
      dbscan,
      isMineral,
      isGeyser,
      (.|), 
      fromEnum',
      toEnum' )
import Agent (Observation)
import Data.ProtoLens (decodeMessage)
import qualified Data.ByteString as B
import Control.Monad.IO.Class (liftIO)
import qualified Utils
import Lens.Micro
import Lens.Micro.Extras(view)

import Data.Ord
import Data.List
import Data.Function
import qualified Data.Map as Map
import Proto.S2clientprotocol.Sc2api (ResponseGameInfo)
import Proto.S2clientprotocol.Common as C

import Data.Char(chr)
import Utils(TilePos, tilePos, tileX, tileY, distSquaredTile, distSquared, distManhattan)
import Data.Foldable (minimumBy)
import Data.Maybe (fromJust)

import Data.Monoid

fromEither :: Either String a -> a
fromEither (Left err) = error err
fromEither (Right val) = val

loadTestsData :: IO (Observation, ResponseGameInfo)
loadTestsData = do
  obs <- B.readFile "test/data/obs0"
  gi <- B.readFile "test/data/gameinfo"

  return (fromEither (decodeMessage obs :: Either String Observation), fromEither (decodeMessage gi :: Either String ResponseGameInfo))

main :: IO ()
main = hspec spec

markClusters :: MapClusters -> Grid -> Grid
markClusters labels grid = foldl' mark grid (Map.toList labels) where
  mark g (p, Cluster n) = updatePixel g (tilePos $ p ^. #pos) 'x'
  mark g (p, Noise) = updatePixel g (tilePos $ p ^. #pos) 'X'

unitsBoundingBox :: [Units.Unit] -> (TilePos, TilePos)
unitsBoundingBox cluster = ((tileX minX, tileY minY), (tileX maxX, tileY maxY))
  where
    minX = minimumBy (compare `on` view #x) points
    maxX = maximumBy (compare `on` view #x) points

    minY = minimumBy (compare `on` view #y) points
    maxY = maximumBy (compare `on` view #y) points

    points = view #pos <$> cluster

fprtnBoundingBox :: (TilePos, TilePos) -> (Footprint, TilePos)
fprtnBoundingBox (origin@(tlX, tlY), (brX, brY)) = (Footprint {pixels = topH ++ bottomH ++ leftV ++ rightV}, origin)
  where
    topH = [(x, 0, '#') | x <- [0..(brX - tlX)]]
    bottomH = [(x, brY - tlY, '#') | x <- [0..(brX - tlX)]]
    leftV = [(0, y, '#') | y <- [1..(brY - tlY)]]
    rightV = [(brX - tlX, y, '#') | y <- [1..(brY - tlY)]]

findNexusPlacementPos :: Grid -> Grid -> [Units.Unit] -> Maybe TilePos
findNexusPlacementPos grid heightMap cluster = gridBfs grid (tilePos . view #pos . head $ cluster) canPlaceDist3 notInBb
  where
    ((tlX, tlY), (brX, brY)) = unitsBoundingBox cluster
    --notInBb (x, y) = x >= tlX && x <= brX && y >= tlY && y <= brY
    notInBb = const False
    clusterTiles = tilePos . view #pos <$> cluster
    canPlaceDist3 p = all (\c -> distSquaredTile p c >= 6*6 && distSquaredTile p c < 9*9) clusterTiles && canPlaceBuilding grid heightMap p (getFootprint ProtossNexus)

gridPlace :: Unit -> Grid -> Grid
gridPlace u g = addMark g (getFootprint . toEnum . fromIntegral . view #unitType $ u) (tilePos $ u ^. #pos)

gridUpdate :: Observation -> Grid -> Grid
gridUpdate obs grid = foldl (\acc (fp, pos) -> addMark acc fp pos) grid (getFootprints <$> units) where -- `Utils.dbg` ("gridUpdate" ++ show fp ++ " " ++ show pos)) grid (getFootprints <$> units)
  --units = filter (\u -> toEnum' (u ^. #unitType) /= ProtossProbe) (obs ^. (#rawData . #units))
  units = obs ^. (#rawData . #units)
  --getFootprints :: Units.Unit -> (Footprint, (Int, Int))
  getFootprints u = (getFootprint (toEnum' $ u ^. #unitType), tilePos $ u ^. #pos) -- `Utils.dbg` ("getFootPrint " ++ show (toEnum' (u ^. #unitType) :: UnitTypeId) ++ " " ++ show (tilePos $ u ^. #pos))

gridFoldr :: (a -> Grid -> Grid) -> [a] -> Grid -> Grid
gridFoldr f g xs = foldr f xs g

-- gridFoldr :: (a -> Endo Grid) -> [a] -> Endo Grid
-- gridFoldr f = mconcat . map f

spec :: Spec
spec =
  beforeAll loadTestsData
  $ do
  describe "Observation test suite" $ do
    it "Units tests" $ \(obs, _) -> do
      runC (obsUnitsC obs) `shouldSatisfy` (not . null) `Utils.dbg` "test !!!!!!!!!!!!!!!!!!!test "

      let mineralFields = runC $ obsUnitsC obs .| unitTypeC NeutralMineralfield
      length mineralFields `shouldBe` 60

    it "dbscan tests" $ \(obs,gi) -> do
      --print obs
      let resourceFields = runC $ obsUnitsC obs .| filterC (\x -> isMineral x || isGeyser x)
          grid = gridFromImage (gi ^. (#startRaw . #placementGrid))
          heights = gridFromImage $ gi ^. (#startRaw . #terrainHeight)

          -- calculate clusters
          marked = dbscan 10 2 resourceFields
          clusters = groupBy ((==) `on` snd) $ sortOn snd $ Map.toList marked
          clusteredUnits = map (map fst) clusters
          closestCluster = snd $ minimumBy (compare `on` (\(u, l) -> distSquared (u ^. #pos :: C.Point) (nexus ^. #pos))) (Map.toList marked)

          --start location cluster
          nexusCluster = [u | (u, cl0) <- Map.toList marked, cl0 == closestCluster ]
          nexus = findNexus obs

          bbFootPrints = fprtnBoundingBox . unitsBoundingBox <$> clusteredUnits

          expands = findNexusPlacementPos grid' heights <$> clusteredUnits

          grid' = markClusters marked . gridPlace nexus $ grid 
          grid'' = gridFoldr (\x g -> updatePixel g (fromJust x) 'c') expands . gridFoldr (\(fp, p) g -> addMark g fp p) bbFootPrints $ grid'

          nexusPlacement :: Maybe TilePos
          nexusPlacement = findNexusPlacementPos grid' heights nexusCluster

          nexusPlacementGt :: Maybe TilePos
          nexusPlacementGt = Just . tilePos . view #pos $ nexus

      print $ foldl (\acc cl  -> (show . snd . head $ cl, length cl): acc ) [] clusters
      print $ bbFootPrints
      --mapM_ (\x -> print $ sqrt $ distSquaredTile (tilePos . view #pos $ x) nexusPos) nexusCluster
      --mapM_ (\x -> print $ distManhattan (tilePos . view #pos $ x) nexusPos) nexusCluster
      --print $ "======================================================"
      --mapM_ (\x -> print $ sqrt $ distSquaredTile (tilePos . view #pos $ x) (fromJust nexusPlacement)) nexusCluster
      --mapM_ (\x -> print $ distManhattan (tilePos . view #pos $ x) (fromJust nexusPlacement)) nexusCluster
      --print $ "======================================================"
      --print $ sum $ [distSquaredTile (tilePos . view #pos $ x) nexusPos | x <- nexusCluster]
      --print $ sum $ [distSquaredTile (tilePos . view #pos $ x) (fromJust nexusPlacement) | x <- nexusCluster]
      gridToFile "outgrid.txt" grid''

      length marked `shouldBe` length resourceFields
      length clusters `shouldBe` 16
      nexusPlacement `shouldBe` nexusPlacementGt
      print $ "!!!!!!!!!!!" ++ show nexusPlacement
      return ()

    --it "Clusterization tests" $ \obs -> do
    --  --print obs
    --  let clusters = clusterUnits obs NeutralMineralfield `Utils.dbg` "test !!!!!!!!!!!!!!!!!!!test "
    --  length clusters `shouldBe` 16

-- main :: IO ()
-- main = do
--     let mapRows =
--             [ "###################################################"
--             , "#                                                 #"
--             , "#                                                 #"
--             , "#                                                 #"
--             , "#                                                 #"
--             , "#                                                 #"
--             , "#                                                 #"
--             , "#                                                 #"
--             , "#                                                 #"
--             , "#                                                 #"
--             , "#                                                 #"
--             , "#                                                 #"
--             , "#                                                 #"
--             , "#                                                 #"
--             , "#                                                 #"
--             , "#                                                 #"
--             , "###################################################"
--             ]
--     let buildingStr = unlines
--             [ " bb"
--             , "bcb"
--             , "bbb"
--             ]
--     let buildingFootprint = getFootprint ProtossNexus
--     print buildingFootprint
-- 
--     let myMap = createGrid mapRows
--     let maybePlacementPoint = findPlacementPoint myMap buildingFootprint  (3,3)
--     case maybePlacementPoint of
--         Just placementPoint -> do
--             putStrLn "\nPlacement Point:"
--             putStrLn $ "Found placement point: " ++ show placementPoint
-- 
--             let placedMap = return (addMark myMap buildingFootprint placementPoint) >>= \x -> updatePixel x placementPoint 'c'
--             putStrLn "\nPlaced Map:"
--             printGrid placedMap
-- 
--         Nothing -> putStrLn "No suitable placement point found"
