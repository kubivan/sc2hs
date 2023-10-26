{-# LANGUAGE OverloadedLabels #-}

module Grid(gridFromImage, gridToString, createGrid, Grid, findPlacementPoint, findPlacementPointInRadius, addMark, printGrid, writeGridToFile, updatePixel) where

import qualified Data.Vector as V
import Data.Char (chr)
import Data.List ( elemIndices, foldl' )
import Data.ByteString (ByteString, unpack)
import qualified Data.ByteString as BS
import Data.Word (Word8)
import Data.Bits
import qualified Data.Text.IO as TIO
import qualified Data.Text as T

import Lens.Micro ( (^.) )

import qualified Data.Set as Set

import Debug.Trace
import Control.Monad (join)
import qualified Proto.S2clientprotocol.Common as P
import qualified Proto.S2clientprotocol.Common_Fields as P

import Footprint
import Utils (distSquared, tilePos, fromTuple, dbg)
import Data.Maybe (isJust)

dbg = flip trace

type Grid = V.Vector (V.Vector Char)
type TilePos = (Int, Int)

gridH :: Grid -> Int
gridH = V.length

gridW :: Grid -> Int
gridW = V.length . V.head

gridPixel :: Grid -> TilePos -> Char
gridPixel g (x, y) = (g V.! y) V.! x

gridPixelSafe :: Grid -> TilePos -> Maybe Char
gridPixelSafe g (x, y) = g V.!? y >>= (V.!? x)

createGrid :: [String] -> Grid
createGrid rows = V.fromList [V.fromList row | row <- rows]

gridToString :: Grid -> String
gridToString = V.foldr (\row acc -> V.foldr (:) "\n" row ++ acc) ""

gridFromImage :: P.ImageData -> Grid
gridFromImage image = decodeImageData width height bpp bs
  where
    width = fromIntegral $ image ^. (P.size . P.x)
    height = fromIntegral $ image ^. (P.size . P.y)
    bpp = fromIntegral $ image ^. P.bitsPerPixel
    bs = image ^. P.data' :: BS.ByteString

writeGridToFile :: FilePath -> Grid -> IO ()
writeGridToFile filePath grid = do
  let mirrored = V.toList <$> (V.reverse . mirrorGrid $ grid)
  let flattened = unlines . V.toList $ mirrored
  writeFile filePath flattened

mirrorGrid :: Grid -> Grid
mirrorGrid grid =
    V.fromList [V.fromList [grid V.! j V.! i | j <- [0..V.length grid - 1]] | i <- [0..V.length (V.head grid) - 1]]

printGrid grid = V.sequence_ $ putStrLn . V.toList <$> (V.reverse . mirrorGrid $ grid)

-- Decode a single byte into a list of bits.
decodeByte :: Word8 -> [Bool]
decodeByte byte = [testBit byte i | i <- [0..7]]

decodeImageData :: Int -> Int -> Int -> BS.ByteString -> Grid
decodeImageData width height bitsPerPixel bytes = V.generate height rowGenerator
  where
    rowGenerator i = V.generate width (pixelGenerator i)
    pixelGenerator i j = if (byte `shiftR` (7 - bitIndex)) .&. 1 == 1 then ' ' else '#'
      where
          offset = (j * width + i) * bitsPerPixel
          byteIndex = offset `div` 8
          bitIndex = offset `rem` 8
          byte = BS.index bytes byteIndex :: Word8

canPlaceBuilding :: Grid -> (Int, Int) -> Footprint -> Bool
canPlaceBuilding grid (cx, cy) (Footprint building _)  =

    all pixelOk building where
      pixelOk (x, y) = case gridPixelSafe grid (cx + x, cy + y) of
        Nothing -> False
        Just p -> p == ' '

findPlacementPoint :: Grid -> Footprint -> (Int, Int) -> Maybe (Int, Int)
findPlacementPoint grid footprint pivotPoint =
    bfs [pivotPoint] (Set.singleton pivotPoint)
  where
    bfs [] _ = Nothing
    bfs (top : rest) visited
        | canPlaceBuilding grid top footprint = Just top
        | otherwise = bfs (rest ++ newPoints) newVisited
      where
        newPoints = filter (`Set.notMember` visited) (neighbors top)
        newVisited = Set.union visited (Set.fromList newPoints)

    neighbors :: (Int, Int) -> [(Int, Int)]
    neighbors (x, y) =
        [ (x + dx, y + dy)
        | dx <- [-1, 0, 1]
        , dy <- [-1, 0, 1]
        , dx /= 0 || dy /= 0  -- Exclude points on the same vertical line
        , x + dx >= 0 && x + dx < V.length grid
        , y + dy >= 0 && y + dy < V.length (V.head grid)
        ]

findPlacementPointInRadius :: Grid -> Footprint -> (Int, Int) -> Float -> Maybe (Int, Int)
findPlacementPointInRadius grid footprint pivotPoint radius =
    bfs [pivotPoint] (Set.singleton pivotPoint)
  where
    bfs [] _ = Nothing
    bfs (top : rest) visited
        | canPlaceBuilding grid top footprint = Just top
        | otherwise = bfs (rest ++ newPoints) newVisited
      where
        newPoints = filter (\p -> distSquared (fromTuple pivotPoint) (fromTuple p) <= (radius * radius)) $ filter (`Set.notMember` visited) (neighbors top)
        newVisited = Set.union visited (Set.fromList newPoints)

    neighbors :: (Int, Int) -> [(Int, Int)]
    neighbors (x, y) =
        [ (x + dx, y + dy)
        | dx <- [-1, 0, 1]
        , dy <- [-1, 0, 1]
        , dx /= 0 || dy /= 0  -- Exclude points on the same vertical line
        , isJust $ gridPixelSafe grid (x + dx, y + dy)
        ]

-- Place a building footprint on the grid if possible at the given placement point
addMark :: Grid -> Footprint -> (Int, Int) -> Grid
addMark grid (Footprint deltas mark) (cx, cy) =
    foldl (\accGrid (x, y) -> updatePixel accGrid (cx + x, cy + y) mark) grid deltas

-- Update a cell in the Grid
updatePixel :: Grid -> (Int, Int) -> Char -> Grid
updatePixel grid (i, j) value = grid V.// [(j, (grid V.! j) V.// [(i, value)])]
