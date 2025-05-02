{-# OPTIONS -Wall #-}
-- {-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Grid.Utils (
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

import Grid.Algo (GridBfsRes (..), gridBfs)
import Grid.Core

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
import Utils (TilePos, dbg, distSquared, fromTuple)

import Data.Sequence qualified as Seq
import Debug.Trace (trace)
import UnitTypeId (UnitTypeId)

smartTransition :: Grid -> [(Char, Char)] -> TilePos -> Seq.Seq TilePos
smartTransition grid transitions pos@(x, y) = Seq.fromList $ filter passTransitions allAdjacent
  where
    pixelFrom = gridPixel grid pos
    allAdjacent =
        [ (x + dx, y + dy)
        | (dx, dy) <- [(-1, 0), (1, 0), (0, -1), (0, 1)]
        , let pixel = gridPixelSafe grid (x + dx, y + dy)
        , isJust pixel
        ]
    passTransitions p = isJust $ find (canTransit p) transitions
    canTransit p (f, t) = res -- `Utils.dbg` (show pos ++ " : " ++show p ++ " " ++ show res ++ " :transition from " ++ show pixelFrom ++ " to " ++ show (gridPixel grid p))
      where
        res = pixelFrom == f && gridPixel grid p == t

getAllNeigbors :: Grid -> TilePos -> Seq.Seq TilePos
getAllNeigbors grid (x, y) = res -- `Utils.dbg` ("ns of " ++ show (x,y) ++ " is " ++ show (Seq.length res))
  where
    res =
        Seq.fromList
            [ (x + dx, y + dy)
            | dx <- [-1, 0, 1]
            , dy <- [-1, 0, 1]
            , dx /= 0 || dy /= 0 -- Exclude points on the same vertical line
            , let pixel = gridPixelSafe grid (x + dx, y + dy)
            , isJust pixel -- pixel /= Just '#'
            ]

getAllNotSharpNeigbors :: Grid -> TilePos -> Seq.Seq TilePos
getAllNotSharpNeigbors grid (x, y) =
    Seq.fromList
        [ (x + dx, y + dy)
        | dx <- [-1, 0, 1]
        , dy <- [-1, 0, 1]
        , dx /= 0 || dy /= 0 -- Exclude points on the same vertical line
        , let pixel = gridPixelSafe grid (x + dx, y + dy)
        , pixel /= Just '#'
        ]

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
    pixelOk (x, y, _) = case gridPixelSafe grid (cx + x, cy + y) of
        Nothing -> False
        Just p -> p == ' ' -- TODO: now . has multiple meanings, rework
    sameHeight pixels = all (== head pixelHeights) (tail pixelHeights)
      where
        pixelHeights = [gridPixel heightMap pos | (x, y, _) <- pixels, let pos = (cx + x, cy + y)]

findPlacementPoint :: Grid -> Grid -> Footprint -> TilePos -> (TilePos -> Bool) -> Maybe TilePos
findPlacementPoint grid heightMap footprint start acceptanceCriteria = bfsRes $ gridBfs grid start (getAllNeigbors grid) acceptance (const False)
  where
    acceptance p = canPlaceBuilding grid heightMap p footprint && acceptanceCriteria p

findPlacementPointInRadius :: Grid -> Grid -> Footprint -> TilePos -> Float -> Maybe TilePos
findPlacementPointInRadius grid heightMap footprint start radius =
    bfsRes $ gridBfs grid start (getAllNeigbors grid) acceptWhen terminateWhen
  where
    acceptWhen p = canPlaceBuilding grid heightMap p footprint
    terminateWhen p = distSquared (fromTuple start) (fromTuple p) > (radius * radius)