--{-# LANGUAGE FlexibleInstances #-}

module Grid
  ( gridFromImage,
    gridToStr,
    createGrid,
    Grid,
    findPlacementPoint,
    findPlacementPointInRadius,
    canPlaceBuilding,
    gridBfs,
    gridPlace,
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
import Utils (distSquared, tilePos, fromTuple, dbg, TilePos)
import Data.Maybe (isJust, fromJust)
import Data.Foldable (toList)

import qualified Data.Sequence as Seq -- (Seq (..), empty, (|>))
import UnitTypeId (UnitTypeId)

type Grid = V.Vector (V.Vector Char)

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
canPlaceBuilding grid heightMap (cx, cy) (Footprint pixels) =
    all pixelOk pixels && sameHeight pixels where
      pixelOk (x, y, _) = case gridPixelSafe grid (cx + x, cy + y) of
        Nothing -> False
        Just p -> p /= '#'
      sameHeight pixels = all (== head pixelHeights) (tail pixelHeights) where
        pixelHeights = [gridPixel heightMap pos | (x, y, _) <- pixels, let pos = (cx + x, cy + y) ]

findPlacementPointInRadius :: Grid -> Grid -> Footprint -> TilePos -> Float -> Maybe TilePos
findPlacementPointInRadius grid heightMap footprint start radius =
  gridBfs grid start acceptWhen terminateWhen
    where
      acceptWhen p = canPlaceBuilding grid heightMap p footprint
      terminateWhen p = distSquared (fromTuple start) (fromTuple p) > (radius * radius)

findPlacementPoint :: Grid -> Grid -> Footprint -> TilePos -> (TilePos -> Bool) -> Maybe TilePos
findPlacementPoint grid heightMap footprint start acceptanceCriteria = gridBfs grid start acceptance (const False) where
    acceptance p = canPlaceBuilding grid heightMap p footprint && acceptanceCriteria p

gridBfs :: Grid -> TilePos -> (TilePos -> Bool) -> (TilePos -> Bool) -> Maybe TilePos
gridBfs grid start acceptanceCriteria terminationCriteria =
    bfs (Seq.singleton start) (Set.singleton start)
  where
    bfs Seq.Empty _ = Nothing
    bfs (top Seq.:<| rest) visited
        | acceptanceCriteria top = Just top `Utils.dbg` ("gridBfs ended. visited: " ++ show (length visited))
        | terminationCriteria top = Nothing
        | otherwise = bfs (rest Seq.>< newPoints) newVisited
      where
        newPoints = Seq.filter (`Set.notMember` visited) (neighbors top)
        newVisited = Set.union visited (Set.fromList . toList $ newPoints)

    neighbors :: TilePos -> Seq.Seq TilePos
    neighbors (x, y) = Seq.fromList
        [ (x + dx, y + dy)
        | dx <- [-1, 0, 1]
        , dy <- [-1, 0, 1]
        , dx /= 0 || dy /= 0  -- Exclude points on the same vertical line
        , let pixel = gridPixelSafe grid (x + dx, y + dy)
        , isJust pixel --pixel /= Just '#'
        ]

-- Place a building footprint on the grid if possible at the given placement point
addMark :: Grid -> Footprint -> TilePos -> Grid
addMark grid (Footprint pixels) (cx, cy) =
    foldl (\accGrid (x, y, mark) -> updatePixel accGrid (cx + x, cy + y) mark) grid pixels

gridPlace :: UnitTypeId -> TilePos -> Grid -> Grid
gridPlace u (cx, cy) g = foldl (\accGrid (x, y, mark) -> updatePixel accGrid (cx + x, cy + y) mark) g ptrn
  where
    ptrn = pixels (getFootprint u)

-- Update a cell in the Grid
updatePixel :: Grid -> TilePos -> Char -> Grid
updatePixel grid (i, j) value = grid V.// [(j, (grid V.! j) V.// [(i, value)])]
