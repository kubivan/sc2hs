{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}

import Conduit
import Grid
import Footprint
import UnitTypeId

import Test.Hspec
import Units
    ( obsUnitsC,
      runC,
      unitTypeC,
      unitsBoundingBox,
      findNexus,
      PointLabel(..),
      MapClusters,
      Unit(..),
      dbscan,
      isMineral,
      isGeyser, findExpands, toEnum' )
import Agent (Observation)
import Data.ProtoLens (decodeMessage)
import qualified Data.ByteString as B
import Lens.Micro ( (^.) )

import Data.Function
import Data.List ( foldl', minimumBy, groupBy, foldl', sortOn )
import qualified Data.Map as Map
import Proto.S2clientprotocol.Sc2api (ResponseGameInfo)
import Proto.S2clientprotocol.Common as C

import Utils(TilePos, tilePos, distSquaredTile, distSquared)
import Data.Maybe (fromJust)
import Lens.Micro.Extras(view)

import Data.Monoid
import Test.Hspec (shouldSatisfy)
import Proto.S2clientprotocol.Debug_Fields (unitType)

fromEither :: Either String a -> a
fromEither (Left err) = error err
fromEither (Right val) = val

loadTestData :: IO (Observation, ResponseGameInfo)
loadTestData = do
  obs <- B.readFile "test/data/obs0"
  gi <- B.readFile "test/data/gameinfo"

  return (fromEither (decodeMessage obs :: Either String Observation), fromEither (decodeMessage gi :: Either String ResponseGameInfo))

markClusters :: MapClusters -> Grid -> Grid
markClusters labels grid = foldl' mark grid (Map.toList labels) where
  mark g (p, Cluster n) = updatePixel g (tilePos $ p ^. #pos) 'x'
  mark g (p, Noise) = updatePixel g (tilePos $ p ^. #pos) 'X'

gridUpdate :: Observation -> Grid -> Grid
gridUpdate obs grid = foldl (\acc (fp, pos) -> addMark acc fp pos) grid (getFootprints <$> units) where -- `Utils.dbg` ("gridUpdate" ++ show fp ++ " " ++ show pos)) grid (getFootprints <$> units)
  --units = filter (\u -> toEnum' (u ^. #unitType) /= ProtossProbe) (obs ^. (#rawData . #units))
  units = obs ^. (#rawData . #units)
  getFootprints :: Units.Unit -> (Footprint, (Int, Int))
  getFootprints u = (getFootprint (toEnum' $ u ^. #unitType), tilePos $ u ^. #pos) -- `Utils.dbg` ("getFootPrint " ++ show (toEnum' (u ^. #unitType) :: UnitTypeId) ++ " " ++ show (tilePos $ u ^. #pos))

spec :: Spec
spec =
  beforeAll loadTestData
  $ do
  describe "Observation test suite" $ do
    it "Units tests" $ \(obs, _) -> do
      runC (obsUnitsC obs) `shouldSatisfy` (not . null)

      let mineralFields = runC $ obsUnitsC obs .| unitTypeC NeutralMineralfield
      length mineralFields `shouldBe` 60

    it "dbscan tests" $ \(obs,gi) -> do
      --print obs
      let resourceFields = runC $ obsUnitsC obs .| filterC (\x -> isMineral x || isGeyser x)
          grid = gridUpdate obs $ gridFromImage (gi ^. (#startRaw . #placementGrid))
          heights = gridFromImage $ gi ^. (#startRaw . #terrainHeight)

          -- calculate clusters
          marked = dbscan 10 2 resourceFields
          clusters = groupBy ((==) `on` snd) $ sortOn snd $ Map.toList marked
          clusteredUnits = map fst <$> clusters
          expands = findExpands obs grid heights

          bbFootPrints = footprintRect . unitsBoundingBox <$> clusteredUnits

          gridWithClusters = appEndo (
            Endo (markClusters marked)
            <> foldMap (\(fp, p) -> Endo (\g -> addMark g fp p)) bbFootPrints)
            grid

          grid' =
           foldl' (flip (gridPlace UnitTypeId.ProtossNexus)) grid expands

      print $ foldl (\acc cl  -> (show . snd . head $ cl, length cl): acc ) [] clusters
      print $ bbFootPrints
      gridToFile "outgrid.txt" grid'

      length marked `shouldBe` length resourceFields
      length expands `shouldBe` 15 -- 16 - already buit start nexus

gridUnitTests :: Spec
gridUnitTests = do
  let testGrid = createGrid
             [ "###################################################"
             , "#                                                 #"
             , "#                                                 #"
             , "#                                                 #"
             , "#                                                 #"
             , "#                                                 #"
             , "#                                                 #"
             , "#                                                 #"
             , "#                                                 #"
             , "#                                                 #"
             , "#                                                 #"
             , "#                                                 #"
             , "#                                                 #"
             , "#                                                 #"
             , "#                                                 #"
             , "#                                                 #"
             , "###################################################"
             ]
  describe "Grid palacement unit tests" $ do
    it "basic" $ do

      let buildingFootprint = getFootprint ProtossNexus
      print buildingFootprint

      let maybePlacementPoint = findPlacementPoint testGrid testGrid buildingFootprint (3,3) (const True)
      maybePlacementPoint `shouldNotBe` Nothing

main :: IO ()
main = hspec $ spec >> gridUnitTests