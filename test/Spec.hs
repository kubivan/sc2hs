{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}

import Conduit
import Footprint
import Grid.Grid
import SC2.Ids.UnitTypeId

import qualified Data.ByteString as B
import Data.ProtoLens (decodeMessage)
import Lens.Micro ((^.))
import Observation
import Test.Hspec
import Units

import Debug.Trace (trace, traceShow, traceShowId, traceM)

import Data.Function
import Data.List (find, sort, foldl', groupBy, minimumBy, sortOn)
import qualified Data.Map as Map
import Proto.S2clientprotocol.Common as C
import Proto.S2clientprotocol.Sc2api (ResponseGameInfo)

import Data.Maybe (fromJust, fromMaybe, isJust)
import Lens.Micro.Extras (view)
import Utils (TilePos, dbg, distManhattan, distSquared, tilePos)

import Data.Monoid
import Proto.S2clientprotocol.Debug_Fields (unitType)
import Test.Hspec (shouldSatisfy)
import Units (isBuildingType)

-- floodfill

import qualified Data.Set as Set
import qualified Data.Sequence as Seq

import Control.Monad (guard, mplus)
import Control.Monad.Trans.Maybe
import Control.Monad.State
import Control.Monad.Writer

import Data.Char (digitToInt, intToDigit)
import Data.Maybe (catMaybes, fromMaybe)

import qualified Data.Set as Set

import Control.Monad (msum)

import TestGrid(gridUnitTests)

import Proto.S2clientprotocol.Raw_Fields (placementGrid, pathingGrid)
import qualified Data.Vector.Unboxed as VU

fromEither :: Either String a -> a
fromEither (Left err) = error err
fromEither (Right val) = val

loadTestData :: IO (Observation, ResponseGameInfo)
loadTestData = do
    obs <- B.readFile "test/data/obs0"
    gi <- B.readFile "test/data/gameinfo"

    return
        ( fromEither (decodeMessage obs :: Either String Observation)
        , fromEither (decodeMessage gi :: Either String ResponseGameInfo)
        )

markClusters :: MapClusters -> Grid -> Grid
markClusters labels grid = foldl' mark grid (Map.toList labels)
  where
    mark g (p, Cluster n) = gridSetPixel g (tilePos $ p ^. #pos) 'x'
    mark g (p, Noise) = gridSetPixel g (tilePos $ p ^. #pos) 'X'

spec :: Spec
spec =
    beforeAll loadTestData $
        do
            describe "Observation" $ do
                it "check_object_orientation" $ \(obs, gi) -> do
                    let nexus = head $ runC $ unitsSelf obs .| unitTypeC ProtossNexus
                        nexusPos@(x,y) = tilePos $ view #pos $ nexus
                        image = gi ^. (#startRaw . #pathingGrid)
                        grid@(w, h, bs) = gridFromImage image
                        -- wallC = charToWord8 '#'
                        gridWithNexus = gridPlace grid (toEnum' $ nexus ^. #unitType) (tilePos $ nexus ^. #pos)

                        nexusCenter = gridPixel grid nexusPos

                    print $ "nexus Pos: " ++ show nexusPos

                    gridPixel gridWithNexus nexusPos `shouldBe` 'c'
                    gridToFile "check_object_orientation.txt" gridWithNexus

                    nexusPos `shouldBe` (57, 60)
                    w  `shouldBe` 224
                    h  `shouldBe` 224
                    w * h `shouldBe` VU.length bs



                it "Units tests" $ \(obs, _) -> do
                    runC (obsUnitsC obs) `shouldSatisfy` (not . null)

                    let mineralFields = runC $ obsUnitsC obs .| unitTypeC NeutralMineralfield
                    length mineralFields `shouldBe` 60

                    isBuildingType ProtossNexus `shouldBe` True
                    isBuildingType ProtossAssimilator `shouldBe` True

                    isBuildingType ProtossCyberneticscore `shouldBe` True

                    isBuildingType ProtossStalker `shouldBe` False

                it "dbscan tests" $ \(obs, gi) -> do
                    -- print obs
                    let resourceFields = runC $ obsUnitsC obs .| filterC (\x -> isMineral x || isGeyser x)
                        grid = gridUpdate obs $ gridFromImage (gi ^. (#startRaw . #placementGrid))
                        heights = gridFromImage $ gi ^. (#startRaw . #terrainHeight)

                        -- calculate clusters
                        marked = dbscan 10 2 resourceFields
                        clusters = groupBy ((==) `on` snd) $ sortOn snd $ Map.toList marked
                        clusteredUnits = map fst <$> clusters
                        expands = findExpands obs grid heights

                        bbFootPrints = footprintRect . unitsBoundingBox <$> clusteredUnits

                        gridWithClusters =
                            appEndo
                                ( Endo (markClusters marked)
                                    <> foldMap (\(fp, p) -> Endo (\g -> addMark g fp p)) bbFootPrints
                                )
                                grid

                        grid' =
                            foldl' (`gridPlace` ProtossNexus) grid expands

                    print $ foldl' (\acc cl -> (show . snd . head $ cl, length cl) : acc) [] clusters
                    print $ bbFootPrints
                    gridToFile "outgrid_with_clasters.txt" gridWithClusters

                    length marked `shouldBe` length resourceFields
                    length expands `shouldBe` 15 -- 16 - already buit start nexus
                it "grid_segmentation" $ \(obs, gi) -> do
                    let
                        nexusPos = tilePos $ view #pos $ head $ runC $ unitsSelf obs .| unitTypeC ProtossNexus
                        grid = gridFromImage (gi ^. (#startRaw . #pathingGrid))
                        (rays, grid') = findAllChokePoints grid

                        res = gridSegment grid'
                        charLabels :: [Char]
                        charLabels = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

                        resWithCharId = zip charLabels (map snd res)

                        grid'' = foldl (\gridAcc (id, region) -> foldl'(\ga p -> gridSetPixel ga p id ) gridAcc (Set.toList region) ) grid' resWithCharId

                    print $ "nexusPos pixel: " ++ show nexusPos ++ " " ++ show (gridPixel grid nexusPos)
                    print $ "grid segmented into " ++ show (length res)
                    print $ "grid segmented into " ++ show (foldl' (\a (id, region) -> Set.size region : a ) [] res)
                    gridToFile "outgrid_segmented_start.txt" grid''
                it "grid_split_to_regions" $ \(obs, gi) -> do
                    let
                        nexusPos = tilePos $ view #pos $ head $ runC $ unitsSelf obs .| unitTypeC ProtossNexus
                        enemyPos = tilePos $ enemyBaseLocation gi obs
                        grid = gridFromImage (gi ^. (#startRaw . #pathingGrid))
                        bases = gi ^. (#startRaw . #startLocations)

                        (rays, grid') = findAllChokePoints grid

                        regions = gridSegment grid'
                        regionLookup = buildRegionLookup regions
                        regionGraph = buildRegionGraph regions

                        nexusRegion = Map.lookup nexusPos regionLookup

                    -- print $ "enemy start " ++ show enemyPos ++ " " ++ show (Map.lookup enemyPos regionLookup)
                    print $ "player start " ++ show nexusPos ++ " " ++ show nexusRegion
                    nexusRegion `shouldBe` Just 2 --just fix the value to catch regression
                    print $ "bases " ++ show bases


                it "check volumes" $ \(obs, gi) -> do
                    let grid =
                            gridFromLines
                                [ "########"
                                , "#      #"
                                , "#******#"
                                , "#      #"
                                , "#      #"
                                , "#      #"
                                , "########"
                                ]

                        grid2 =
                            gridFromLines
                                [ "#####################"
                                , "#############       #"
                                , "########   *        #"
                                , "#######   *         #"
                                , "######   *          #"
                                , "#####   *           #"
                                , "#####  *            #"
                                , "##### *             #"
                                , "#####*              #"
                                , "#####               #"
                                , "#####################"
                                ]
                        grid_to_segment =
                            gridFromLines
                                [ "#####################"
                                , "#############       #"
                                , "########            #"
                                , "#######             #"
                                , "######              #"
                                , "#####               #"
                                , "#####               #"
                                , "#####               #"
                                , "#####               #"
                                , "#####               #"
                                , "#####################"
                                ]
                        (chokes, grid') = findAllChokePoints grid_to_segment
                        -- rays = findAllChokePoints grid_to_segment

                        ray2 = [(11, 2), (10, 3), (9, 4), (8, 5), (7, 6), (6, 7)]
                        res2 = checkVolumes grid2 ray2 15

                    checkVolumes grid2 ray2 15 `shouldBe` True
                    checkVolumes grid2 ray2 16 `shouldBe` False

                    gridToFile "grid_to_segment.txt" grid'
                it "findFirstChoke" $ \(obs, gi) -> do
                    let
                        grid =
                            gridFromLines
                                [ "#################################"
                                , "#             ###################"
                                , "#             ###################"
                                , "#######       ###################"
                                , "##  ###                         #"
                                , "#    ##                         #"
                                , "#     ##                        #"
                                , "#      ###                      #"
                                , "##      ###                     #"
                                , "###      ######                 #"
                                , "## #                            #"
                                , "#####                           #"
                                , "######                          #"
                                , "######                          #"
                                , "######                          #"
                                , "######                          #"
                                , "######                          #"
                                , "#################################"
                                ]
                        --grid = gridFromImage (gi ^. (#startRaw . #pathingGrid))
                        openCells = [(x, y) | y <- [0 .. gridH grid - 1], x <- [0 .. gridW grid - 1], gridPixel grid (x, y) /= '#']
                        minVolume = 10

                        Just (ray, va, vb) = msum $ tryFind <$> openCells
                        tryFind pos = do
                            ray <- findChokePoint grid 15 pos
                            let (Just a, Just b) = gridSplitByRay grid 1000 ray
                                volumeA = Set.size a
                                volumeB = Set.size b
                            guard (volumeA >= minVolume && volumeB >= minVolume) `Utils.dbg` ("volumes " ++ show (volumeA, volumeB))
                            return $ (ray, volumeA, volumeB)

                        grid' = gridPlaceRay grid ray

                    print (ray, va, vb)
                    gridToFile "outgrid_with_first_choke.txt" grid'

                it "findAllChokes" $ \(obs, gi) -> do
                    let
                        grid = gridFromImage (gi ^. (#startRaw . #pathingGrid))
                        --Just (chokes, (grid', rays, _)) = findAllChokePoints grid
                        --(grid', rays) = findAllChokePoints grid
                        (rays, grid') = findAllChokePoints grid
                        grid'' = foldl' (\gridA p -> gridSetPixel gridA p 'c') grid' [(136,36),(137,36),(138,36),(139,36),(140,36),(141,36)]

                    gridToFile "outgrid_with_all_chokes.txt" grid''
                    print $ "found " ++ show (length rays) ++ " chokes"

main :: IO ()
main = hspec $ spec >> gridUnitTests
