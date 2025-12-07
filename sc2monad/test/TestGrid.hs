module TestGrid (gridUnitTests) where

import Footprint
import SC2.Grid
import SC2.Ids.UnitTypeId
import Test.Hspec
import Data.List (sort)
import Data.Maybe (fromJust)
import qualified Data.Set as Set


gridUnitTests :: Spec
gridUnitTests = do
    let testGrid =
            gridFromLines
                [ "###################################################"
                , "##                                   ##############"
                , "#                                    ##############"
                , "#                                    ##############"
                , "#                                    ##############"
                , "#                                    ##############"
                , "#                                                 #"
                , "#                                                 #"
                , "#                                                 #"
                , "#                                                 #"
                , "#                                                 #"
                , "#                                                 #"
                , "#                                                 #"
                , "#                                                 #"
                , "#####                                             #"
                , "#####                                             #"
                , "###################################################"
                ]
    describe "Grid unit tests" $ do
        it "basic" $ do
            gridToFile "test_grid.txt" testGrid
            content <- readFile "test_grid.txt"
            let grid' = gridFromLines (lines content)

            grid' `shouldBe` testGrid

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
                    gridFromLines
                        [ "##############"
                        , "##############"
                        , "#######  *   #"
                        , "######  *    #"
                        , "#####  *     #"
                        , "##### *      #"
                        , "#####*       #"
                        , "#####        #"
                        , "#####        #"
                        , "#####        #"
                        , "##############"
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
                    gridFromLines
                        [ "##############"
                        , "##############"
                        , "#######      #"
                        , "######*      #"
                        , "##### *      #"
                        , "##### *      #"
                        , "##### *      #"
                        , "######*      #"
                        , "#######      #"
                        , "#######      #"
                        , "##############"
                        ]

                ray = sort $ [(x, y) | y <- [0 .. gridH grid - 1], x <- [0 .. gridW grid - 1], gridPixel grid (x, y) == '*']
                (Just a1, Just a2) = gridSplitByRay grid 1000 ray
                volumeA1 = Set.size a1
                volumeA2 = Set.size a2
                grid' = gridPlaceRay grid ray

            grid `shouldBe` grid'
            (volumeA1, volumeA2) `shouldBe` (3, 48)

            print $ "resA ray split grid into:" ++ show (volumeA1, volumeA2)