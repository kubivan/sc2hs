{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module TestSegmentation (segmentationIntegrationTests) where

import SC2.Grid

import Control.Monad (forM_)
import Data.HashMap.Strict qualified as HashMap
import Data.List (isSuffixOf)
import Data.Set qualified as Set
import System.Directory (listDirectory)
import Test.Hspec

data SegmentationFixture = SegmentationFixture
    { fixtureName :: FilePath
    , fixtureGrid :: Grid
    }

expectedRegionCount :: FilePath -> Int
expectedRegionCount fileName =
    case fileName of
        "corridor.txt" -> 1
        "map.txt" -> 40
        "map1.txt" -> 39
        _ -> error $ "Missing expected region-count baseline for fixture: " <> fileName

segmentationIntegrationTests :: Spec
segmentationIntegrationTests = do
    fixtures <- runIO loadSegmentationFixtures
    describe "Algo segmentation integration (ASCII fixtures)" $
        forM_ fixtures $ \fx ->
            describe (fixtureName fx) $ do
                it "keeps fixed region count baseline for regression detection" $ do
                    let originalGrid = fixtureGrid fx
                        (_, segmentedGrid) = findAllChokePoints originalGrid
                        regions = gridSegment segmentedGrid

                    length regions `shouldBe` expectedRegionCount (fixtureName fx)

                it "covers all open cells after choke placement and segmentation" $ do
                    let originalGrid = fixtureGrid fx
                        (rays, segmentedGrid) = findAllChokePoints originalGrid
                        regions = gridSegment segmentedGrid
                        raysSet = Set.unions (map Set.fromList rays)
                        openCells =
                            Set.fromList
                                [ (x, y)
                                | y <- [0 .. gridH originalGrid - 1]
                                , x <- [0 .. gridW originalGrid - 1]
                                , gridPixel originalGrid (x, y) /= '#'
                                ]
                        openCellsFromRegions = Set.unions (map snd regions)
                        unsegmentedTiles = Set.difference openCells (Set.union openCellsFromRegions raysSet)

                    unsegmentedTiles `shouldBe` Set.empty

                it "assigns a region id to each passable tile in segmented map" $ do
                    let originalGrid = fixtureGrid fx
                        (_, segmentedGrid) = findAllChokePoints originalGrid
                        regions = gridSegment segmentedGrid
                        regionLookup = buildRegionLookup regions
                        passableTiles =
                            [ (x, y)
                            | y <- [0 .. gridH segmentedGrid - 1]
                            , x <- [0 .. gridW segmentedGrid - 1]
                            , gridPixel segmentedGrid (x, y) /= ' '
                            ]

                    forM_ passableTiles $ \pos ->
                        HashMap.lookup pos regionLookup `shouldSatisfy` maybe False (const True)

                it "buildRegionLookup maps every region tile to its region" $ do
                    let originalGrid = fixtureGrid fx
                        (_, segmentedGrid) = findAllChokePoints originalGrid
                        regions = gridSegment segmentedGrid
                        regionLookup = buildRegionLookup regions

                    forM_ regions $ \(rid, regionTiles) ->
                        forM_ (Set.toList regionTiles) $ \pos ->
                            HashMap.lookup pos regionLookup `shouldBe` Just rid

                it "regionGraphBfs returns singleton path for same start/end region" $ do
                    let originalGrid = fixtureGrid fx
                        (_, segmentedGrid) = findAllChokePoints originalGrid
                        regions = gridSegment segmentedGrid
                        regionLookup = buildRegionLookup regions
                        regionGraph = buildRegionGraph regions regionLookup

                    forM_ (map fst regions) $ \rid ->
                        regionGraphBfs regionGraph rid rid `shouldBe` [rid]

loadSegmentationFixtures :: IO [SegmentationFixture]
loadSegmentationFixtures = do
    let fixtureDir = "test/data/segmentation"
    paths <- listDirectory fixtureDir
    let txtFiles = filter (".txt" `isSuffixOf`) paths
    fixtures <- mapM (loadFixture fixtureDir) txtFiles

    if null fixtures
        then error "No .txt fixtures found in test/data/segmentation"
        else pure fixtures

loadFixture :: FilePath -> FilePath -> IO SegmentationFixture
loadFixture fixtureDir fileName = do
    content <- readFile (fixtureDir <> "/" <> fileName)
    let rows = lines content
    pure $ SegmentationFixture fileName (gridFromLines rows)
