{-# LANGUAGE OverloadedLabels #-}

module Grid(gridFromImage, Footprint(..), createFootprint, createGrid, Grid, findPlacementPoint) where

import qualified Data.Vector as V
import Data.Char (chr)
import Data.List ( elemIndices, foldl' )
import Data.ByteString (ByteString, unpack)
import qualified Data.ByteString as BS
import Data.Word (Word8)
import Data.Bits

import Lens.Micro ( (^.) )

import qualified Data.Set as Set

import Debug.Trace
import Control.Monad (join)
import qualified Proto.S2clientprotocol.Common as P
import qualified Proto.S2clientprotocol.Common_Fields as P

dbg = flip trace

type Grid = V.Vector (V.Vector Char)

data Footprint = Footprint
    { footprint :: [(Int, Int)]
    , mark :: Char
    } deriving(Show, Eq)

createGrid :: [String] -> Grid
createGrid rows = V.fromList [V.fromList row | row <- rows]

findMark :: [Char] -> Char
findMark chars = head [c | c <- chars, c /= ' ' && c /= 'c']

gridFromImage :: P.ImageData -> Grid
gridFromImage image = decodeImageData width height bpp bs
  where
    width = fromIntegral $ image ^. (P.size . P.x)
    height = fromIntegral $ image ^. (P.size . P.y)
    bpp = fromIntegral $ image ^. P.bitsPerPixel
    bs = image ^. P.data' :: BS.ByteString

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

createFootprint :: String -> Footprint
createFootprint str = Footprint footprint (findMark str) where
    rows = lines str
    footprint = [ translatePoint (x, y) (0, 0) (ox, oy) | x <- [0..w-1] , y <- [0..h-1], (str !! (x + y*h)) /= ' ']
    ox = opos - w * oy `dbg` ("opos: " ++ show opos)
    oy = opos `div` w
    w = length.head $ rows
    h = length rows
    opos = head $ elemIndices 'c' (join rows)
    translatePoint (x, y) o0@(x0, y0) o1@(x1, y1) = (xt + x1, yt + y1) where translationVec@(xt, yt) = (x - x0, y - y0)

canPlaceBuilding :: Grid -> (Int, Int) -> Footprint -> Bool
canPlaceBuilding grid (x, y) (Footprint building _) =
    all (\(i, j) -> (grid V.! (x + i)) V.! (y + j) == ' ') building

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

-- Place a building on the grid if possible at the given placement point
placeBuilding :: Grid -> Footprint -> (Int, Int) -> Grid
placeBuilding grid (Footprint buildingMap mark) placementPoint@(x, y) =
    foldl (\m (i, j) -> updateCell m (x + i, y + j) mark) grid buildingMap

-- Update a cell in the Grid
updateCell :: Grid -> (Int, Int) -> Char -> Grid
updateCell grid (i, j) char = grid V.// [(i, (grid V.! i) V.// [(j, char)])]