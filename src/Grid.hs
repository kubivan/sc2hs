--{-# LANGUAGE FlexibleInstances #-}

module Grid
  ( gridFromImage,
    gridToStr,
    createGrid,
    Grid,
    findPlacementPoint,
    findPlacementPointInRadius,
    addMark,
    printGrid,
    gridToFile,
    updatePixel,
    gridPixel,
    gridPixelSafe,
    gridW,
    gridH
  )
where

import qualified Data.Vector as V
import qualified Data.ByteString as BS
import Data.Word (Word8)
import Data.Bits

import Lens.Micro ( (^.) )
import qualified Data.Set as Set

import qualified Proto.S2clientprotocol.Common as P
import qualified Proto.S2clientprotocol.Common_Fields as P

import Footprint
import Utils (distSquared, tilePos, fromTuple, dbg)
import Data.Maybe (isJust)

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

gridToStr :: Grid -> String
gridToStr g = unlines $ V.toList $ V.toList <$> V.reverse g


gridFromImage :: P.ImageData -> Grid
gridFromImage image = decodeImageData width height bpp bs
  where
    width = fromIntegral $ image ^. (P.size . P.x)
    height = fromIntegral $ image ^. (P.size . P.y)
    bpp = fromIntegral $ image ^. P.bitsPerPixel
    bs = image ^. P.data' :: BS.ByteString

gridToFile :: FilePath -> Grid -> IO ()
gridToFile filePath grid =
  writeFile filePath (gridToStr grid)

mirrorGrid :: Grid -> Grid
mirrorGrid g =
    V.fromList [ V.fromList [ g V.! y V.! x | x <- [0..(gridW g - 1)]] | y <- [(gridH g - 1)..0]]

printGrid :: Grid -> IO ()
printGrid = putStrLn . gridToStr

-- Decode a single byte into a list of bits.
decodeByte :: Word8 -> [Bool]
decodeByte byte = [testBit byte i | i <- [0..7]]

decodeImageData :: Int -> Int -> Int -> BS.ByteString -> Grid
decodeImageData width height bpp bytes = V.generate height rowGenerator
  where
    rowGenerator i = V.generate width (pixelGenerator bpp i)
    pixelGenerator 1 j i = if (byte `shiftR` (7 - bitIndex)) .&. 1 == 1 then ' ' else '#'
      where
        offset = j * width + i
        byteIndex = offset `div` 8
        bitIndex = offset `rem` 8
        byte = BS.index bytes byteIndex :: Word8

    pixelGenerator 8 i j = if byte == 1 then ' ' else '#'
      where
        offset = j * width + i
        byte = BS.index bytes offset :: Word8

    pixelGenerator _ i j = undefined

canPlaceBuilding :: Grid -> Grid -> TilePos -> Footprint -> Bool
canPlaceBuilding grid heightMap (cx, cy) (Footprint building _) =
    all pixelOk building && sameHeight building where
      pixelOk (x, y) = case gridPixelSafe grid (cx + x, cy + y) of
        Nothing -> False
        Just p -> p == ' '
      sameHeight building = all (== head pixelHeights) (tail pixelHeights) where
        pixelHeights = [gridPixel heightMap pos | (x, y) <- building, let pos = (cx + x, cy + y) ]

findPlacementPoint :: Grid -> Grid -> Footprint -> TilePos -> Maybe TilePos
findPlacementPoint grid heightMap footprint pivotPoint =
    bfs [pivotPoint] (Set.singleton pivotPoint)
  where
    bfs [] _ = Nothing
    bfs (top : rest) visited
        | canPlaceBuilding grid heightMap top footprint = Just top
        | otherwise = bfs (rest ++ newPoints) newVisited
      where
        newPoints = filter (`Set.notMember` visited) (neighbors top)
        newVisited = Set.union visited (Set.fromList newPoints)

    neighbors :: TilePos -> [TilePos]
    neighbors (x, y) =
        [ (x + dx, y + dy)
        | dx <- [-1, 0, 1]
        , dy <- [-1, 0, 1]
        , dx /= 0 || dy /= 0  -- Exclude points on the same vertical line
        , x + dx >= 0 && x + dx < V.length grid
        , y + dy >= 0 && y + dy < V.length (V.head grid)
        ]

findPlacementPointInRadius :: Grid -> Grid -> Footprint -> TilePos -> Float -> Maybe TilePos
findPlacementPointInRadius grid heightMap footprint pivotPoint radius =
    bfs [pivotPoint] (Set.singleton pivotPoint)
  where
    bfs [] _ = Nothing
    bfs (top : rest) visited
        | canPlaceBuilding grid heightMap top footprint = Just top
        | otherwise = bfs (rest ++ newPoints) newVisited
      where
        newPoints = filter (\p -> distSquared (fromTuple pivotPoint) (fromTuple p) <= (radius * radius)) $ filter (`Set.notMember` visited) (neighbors top)
        newVisited = Set.union visited (Set.fromList newPoints)

    neighbors :: TilePos -> [TilePos]
    neighbors (x, y) =
        [ (x + dx, y + dy)
        | dx <- [-1, 0, 1]
        , dy <- [-1, 0, 1]
        , dx /= 0 || dy /= 0  -- Exclude points on the same vertical line
        , isJust $ gridPixelSafe grid (x + dx, y + dy)
        ]

-- Place a building footprint on the grid if possible at the given placement point
addMark :: Grid -> Footprint -> TilePos -> Grid
addMark grid (Footprint deltas mark) (cx, cy) =
    foldl (\accGrid (x, y) -> updatePixel accGrid (cx + x, cy + y) mark) grid deltas

-- Update a cell in the Grid
updatePixel :: Grid -> TilePos -> Char -> Grid
updatePixel grid (i, j) value = grid V.// [(j, (grid V.! j) V.// [(i, value)])]
