{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}

import Conduit
import Footprint
import Grid
import UnitTypeId

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
import qualified Data.Vector as V

import qualified Data.Set as Set

import Control.Monad (msum)

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

-- Get all valid neighboring tiles
neighbors :: Grid -> TilePos -> [TilePos]
neighbors grid (x, y) =
    filter isValid [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
  where
    isValid (nx, ny) = case gridPixelSafe grid (nx, ny) of
        Just '#' -> False -- Only move on empty spaces
        Just '*' -> False -- Only move on empty spaces
        Nothing -> False
        _ -> True

-- Backtracking-based flood fill for region segmentation
fillRegion :: Grid -> TilePos -> Char -> State Grid Grid
fillRegion grid pos regionID = do
    currentGrid <- get
    case gridPixelSafe currentGrid pos of
        Just ' ' -> do
            let updatedGrid = gridSetPixel currentGrid pos regionID
            put updatedGrid
            mapM_ (\p -> fillRegion updatedGrid p regionID) (neighbors updatedGrid pos)
            return updatedGrid
        _ -> return currentGrid -- Stop if we hit a boundary

-- Find all regions and assign region IDs
segmentGrid :: Grid -> Grid
segmentGrid grid = evalState (exploreAll grid '1') grid
  where
    exploreAll :: Grid -> Char -> State Grid Grid
    exploreAll g regionID = do
        let openCells = [(x, y) | y <- [0 .. gridH g - 1], x <- [0 .. gridW g - 1], gridPixelSafe g (x, y) == Just ' ']
        case openCells of
            [] -> return g
            (start : _) -> do
                filledGrid <- fillRegion g start regionID
                if digitToInt regionID >= 9 then return filledGrid else exploreAll filledGrid (intToDigit (digitToInt regionID + 1))

isValidSegment _ = True

spec :: Spec
spec =
    beforeAll loadTestData $
        do
            describe "Observation test suite" $ do
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
                            foldl' (`gridPlace` UnitTypeId.ProtossNexus) grid expands

                    print $ foldl (\acc cl -> (show . snd . head $ cl, length cl) : acc) [] clusters
                    print $ bbFootPrints
                    gridToFile "outgrid.txt" grid'

                    length marked `shouldBe` length resourceFields
                    length expands `shouldBe` 15 -- 16 - already buit start nexus
                    -- it "ray cast & find choke points" $ \(obs, gi) -> do
                    --     let grid = gridFromImage (gi ^. (#startRaw . #pathingGrid))
                    --         allPixels =
                    --             [ (x, y)
                    --             | x <- [0 .. gridW grid - 1]
                    --             , y <- [0 .. gridH grid - 1]
                    --             , isJust (gridPixelSafe grid (x, y))
                    --             ]
                    --         grid' = foldl (\gridAcc pixel -> maybe gridAcc (gridPlaceRay gridAcc) (findChokePoint grid 12 pixel)) grid allPixels

                --     gridToFile "outgrid_with_choke_points.txt" grid'
                it "grid segmentation" $ \(obs, gi) -> do
                    let
                        grid = gridFromImage (gi ^. (#startRaw . #pathingGrid))
                        -- grid :: Grid
                        -- grid =
                        --     V.fromList
                        --         [ V.fromList "#####"
                        --         , V.fromList "#   #"
                        --         , V.fromList "# # #"
                        --         , V.fromList "#   #"
                        --         , V.fromList "#####"
                        --         ]

                        allPixels =
                            [ (x, y)
                            | x <- [0 .. gridW grid - 1]
                            , y <- [0 .. gridH grid - 1]
                            , isJust (gridPixelSafe grid (x, y))
                            ]
                        firstBlank = find (\p -> ' ' == gridPixel grid p) allPixels
                        grid' = segmentGrid grid

                    gridToFile "outgrid_segmented.txt" grid'
                it "check volumes" $ \(obs, gi) -> do
                    let grid =
                            V.fromList
                                [ V.fromList "########"
                                , V.fromList "#      #"
                                , V.fromList "#******#"
                                , V.fromList "#      #"
                                , V.fromList "#      #"
                                , V.fromList "#      #"
                                , V.fromList "########"
                                ]

                        grid2 =
                            V.fromList
                                [ V.fromList "#####################"
                                , V.fromList "#############       #"
                                , V.fromList "########   *        #"
                                , V.fromList "#######   *         #"
                                , V.fromList "######   *          #"
                                , V.fromList "#####   *           #"
                                , V.fromList "#####  *            #"
                                , V.fromList "##### *             #"
                                , V.fromList "#####*              #"
                                , V.fromList "#####               #"
                                , V.fromList "#####################"
                                ]
                        grid_to_segment =
                            V.fromList
                                [ V.fromList "#####################"
                                , V.fromList "#############       #"
                                , V.fromList "########            #"
                                , V.fromList "#######             #"
                                , V.fromList "######              #"
                                , V.fromList "#####               #"
                                , V.fromList "#####               #"
                                , V.fromList "#####               #"
                                , V.fromList "#####               #"
                                , V.fromList "#####               #"
                                , V.fromList "#####################"
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
                        grid'' = foldl (\gridA p -> gridSetPixel gridA p 'c') grid' [(136,36),(137,36),(138,36),(139,36),(140,36),(141,36)]

                    gridToFile "outgrid_with_all_chokes.txt" grid''
                    print $ "found " ++ show (length rays) ++ " chokes"

gridUnitTests :: Spec
gridUnitTests = do
    let testGrid =
            gridFromLines
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

            let maybePlacementPoint = findPlacementPoint testGrid testGrid buildingFootprint (3, 3) (const True)
            maybePlacementPoint `shouldNotBe` Nothing

    describe "Grid raycast test" $ do
        it "basic raycast" $ do
            let grid =
                    gridFromLines
                        [ "#####"
                        , "#   #"
                        , "# c #"
                        , "#   #"
                        , "#####"
                        ]
                res = gridRaycastTile grid (2, 2) (1, 1)
            head <$> res `shouldBe` Just (4, 4)
        it "choke_point" $ do
            let grid =
                    gridFromLines
                        [ "#################################"
                        , "#             ###################"
                        , "#             ###################"
                        , "#######   a   ###################"
                        , "##  ###                         #"
                        , "#    ##                         #"
                        , "#     ##              b         #"
                        , "#      ###                      #"
                        , "##      ###                     #"
                        , "###      ######                 #"
                        , "## #   c                        #"
                        , "#####                           #"
                        , "######                          #"
                        , "######                          #"
                        , "######                          #"
                        , "######                          #"
                        , "######                          #"
                        , "#################################"
                        ]
                resA = findChokePoint grid 8 (10, 3)
                resB = findChokePoint grid 8 (22, 6)
                resC = findChokePoint grid 6 (7, 10)

                ray = fromJust resA
                (Just a1, Just a2) = gridSplitByRay grid 1000 ray
                volumeA1 = Set.size a1
                volumeA2 = Set.size a2
                grid' = gridPlaceRay grid ray

            print $ "resA ray split grid into:" ++ show (volumeA1, volumeA2)

            print $ "resA:" ++ show resA
            print $ "resB:" ++ show resB
            print $ "resC:" ++ show resC
            (head <$> resA, last <$> resA) `shouldBe` (Just (6, 3), Just (14, 3))
            resB `shouldBe` Nothing
            (head <$> resC, last <$> resC) `shouldBe` (Just (5, 12), Just (9, 8))

            (volumeA1, volumeA2) `shouldBe` (26, 355)

            gridToFile "choke_point.txt" grid'

        it "choke_point_corner" $ do
            let grid =
                    V.fromList
                        [ V.fromList "##############"
                        , V.fromList "##############"
                        , V.fromList "#######  *   #"
                        , V.fromList "######  *    #"
                        , V.fromList "#####  *     #"
                        , V.fromList "##### *      #"
                        , V.fromList "#####*       #"
                        , V.fromList "#####        #"
                        , V.fromList "#####        #"
                        , V.fromList "#####        #"
                        , V.fromList "##############"
                        ]

                ray = sort $ [(x, y) | y <- [0 .. gridH grid - 1], x <- [0 .. gridW grid - 1], gridPixel grid (x, y) == '*']
                (Just a1, Just a2) = gridSplitByRay grid 1000 ray
                volumeA1 = Set.size a1
                volumeA2 = Set.size a2
                grid' = gridPlaceRay grid ray

            grid `shouldBe` grid'
            (volumeA1, volumeA2) `shouldBe` (7, 49)

            print $ "resA ray split grid into:" ++ show (volumeA1, volumeA2)

        it "choke_point_corner1" $ do
            let grid =
                    V.fromList
                        [ V.fromList "##############"
                        , V.fromList "##############"
                        , V.fromList "#######      #"
                        , V.fromList "######*      #"
                        , V.fromList "##### *      #"
                        , V.fromList "##### *      #"
                        , V.fromList "##### *      #"
                        , V.fromList "######*      #"
                        , V.fromList "#######      #"
                        , V.fromList "#######      #"
                        , V.fromList "##############"
                        ]

                ray = sort $ [(x, y) | y <- [0 .. gridH grid - 1], x <- [0 .. gridW grid - 1], gridPixel grid (x, y) == '*']
                (Just a1, Just a2) = gridSplitByRay grid 1000 ray
                volumeA1 = Set.size a1
                volumeA2 = Set.size a2
                grid' = gridPlaceRay grid ray

            grid `shouldBe` grid'
            (volumeA1, volumeA2) `shouldBe` (3, 48)

            print $ "resA ray split grid into:" ++ show (volumeA1, volumeA2)



main :: IO ()
main = hspec $ spec >> gridUnitTests