{-# OPTIONS -Wall #-}
{-# LANGUAGE ImportQualifiedPost #-}

module SC2.Grid.Utils (
    gridFromLines,
    gridToStr,
    findPlacementPoint,
    findPlacementPointInRadius,
    canPlaceBuilding,
    gridMerge,
    printGrid,
    gridToFile,
    pixelIsRamp,
)
where

import SC2.Grid.Algo (GridBfsRes (..), gridBfs, getAllNeighbors)
import SC2.Grid.Core
import SC2.Grid.TilePos
import SC2.Ids.UnitTypeId (UnitTypeId)
import SC2.Geometry

import Data.Bits
import Data.ByteString qualified as BS
import Data.List (find, sort)
import Data.Vector.Unboxed qualified as VU
import Data.Word (Word8)

import Data.Set qualified as Set
import Lens.Micro ((^.))

import Proto.S2clientprotocol.Common qualified as P
import Proto.S2clientprotocol.Common_Fields qualified as P

import Data.Foldable (toList)
import Data.Maybe (isJust)
import Footprint

import Data.Sequence qualified as Seq
import Debug.Trace (trace)

gridFromLines :: [String] -> Grid
gridFromLines rows =
    let h = length rows
        w = length (head rows)
     in (w, h, VU.fromList $ concat rows)

gridToStr (w, h, g) = unlines $ [[g VU.! (x + y * w) | x <- [0 .. w - 1]] | y <- [0 .. h - 1]]

pixelIsRamp :: Char -> Char -> Char
pixelIsRamp placement pathing
    | pathing == ' ' && placement == '#' = '/'
    | otherwise = placement

gridMerge :: (Char -> Char -> Char) -> Grid -> Grid -> Grid
gridMerge pixelFunc placementGrid@(w, h, g1) pathingGrid@(_, _, g2) = (w, h, VU.fromList [pixelFunc pa pb | (pa, pb) <- zip (VU.toList g1) (VU.toList g2)])

gridToFile :: FilePath -> Grid -> IO ()
gridToFile filePath grid =
    writeFile filePath (gridToStr grid)

printGrid :: Grid -> IO ()
printGrid = putStrLn . gridToStr

canPlaceBuilding :: Grid -> Grid -> TilePos -> Footprint -> Bool
canPlaceBuilding grid heightMap (cx, cy) (Footprint pixels) =
    all pixelOk pixels && sameHeight pixels
  where
    pixelOk (x, y, _) = case grid !? (cx + x, cy + y) of
        Nothing -> False
        Just p -> p == ' ' -- TODO: now . has multiple meanings, rework
    sameHeight pixels = all (== head pixelHeights) (tail pixelHeights)
      where
        pixelHeights = [gridPixel heightMap pos | (x, y, _) <- pixels, let pos = (cx + x, cy + y)]

findPlacementPoint :: Grid -> Grid -> Footprint -> TilePos -> (TilePos -> Bool) -> Maybe TilePos
findPlacementPoint grid heightMap footprint start acceptanceCriteria = bfsRes $ gridBfs grid start (getAllNeighbors grid) acceptance (const False)
  where
    acceptance p = canPlaceBuilding grid heightMap p footprint && acceptanceCriteria p

findPlacementPointInRadius :: Grid -> Grid -> Footprint -> TilePos -> Float -> Maybe TilePos
findPlacementPointInRadius grid heightMap footprint start radius =
    bfsRes $ gridBfs grid start (getAllNeighbors grid) acceptWhen terminateWhen
  where
    acceptWhen p = canPlaceBuilding grid heightMap p footprint
    terminateWhen p = distSquared (fromTuple start) (fromTuple p) > (radius * radius)