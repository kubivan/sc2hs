{-# OPTIONS -Wall #-}
{-# LANGUAGE ImportQualifiedPost #-}

module SC2.Grid.Core
  ( Grid
  , GridBase
  , gridFromImage
  , gridW
  , gridH
  , gridPlace
  , addMark
  , removeMark
  , gridSetPixel
  , gridSetPixelForce
  , gridPixel
  , gridPixelSafe
  , (!?)
  , (!)
  )
where

import Footprint
import SC2.Ids.UnitTypeId (UnitTypeId)
import SC2.TilePos

import Data.Bits
import Data.ByteString qualified as BS
import Data.Vector.Unboxed qualified as VU
import Data.Word (Word8)
import Lens.Micro ((^.))

import Proto.S2clientprotocol.Common qualified as P
import Proto.S2clientprotocol.Common_Fields qualified as P

import Debug.Trace (trace)

type GridBase t = (Int, Int, VU.Vector t)

type Grid = GridBase Char

(!?) :: (VU.Unbox t) => GridBase t -> TilePos -> Maybe t
(!?) = gridPixelSafe

(!) :: (VU.Unbox t) => GridBase t -> TilePos -> t
(!) = gridPixel

gridH :: GridBase t -> Int
gridH (_, h, _) = h

gridW :: GridBase t -> Int
gridW (w, _, _) = w

gridPixel :: (VU.Unbox t) => GridBase t -> TilePos -> t
gridPixel (w, h, g) (x, y) = g VU.! index -- `Utils.dbg` ("gridpixel index: " ++ show index  ++ " for " ++ show (w, h, VU.length g))
 where
  index = x + y * w

gridPixelSafe :: (VU.Unbox t) => GridBase t -> TilePos -> Maybe t
gridPixelSafe (w, h, g) (x, y)
  | x < 0 || x >= w = Nothing
  | y < 0 || y >= h = Nothing
  | otherwise = g VU.!? index
 where
  index = x + y * w

-- Update a cell in the Grid
gridSetPixel :: GridBase Char -> TilePos -> Char -> GridBase Char
gridSetPixel grid@(w, h, g) (x, y) value
  | gridPixel grid (x, y) == '#' = grid
  | otherwise = (w, h, g VU.// [(index, value)])
 where
  -- `Utils.dbg` ("gridSetPixel index: " ++ show index  ++ " v:" ++ show value ++ " "  ++ show (x, y) ++ " for " ++ show (w, h, VU.length g)) where
  index = x + y * w

gridSetPixelForce :: (VU.Unbox t) => GridBase t -> TilePos -> t -> GridBase t
gridSetPixelForce grid@(w, h, g) (x, y) value =
  (w, h, g VU.// [(index, value)])
 where
  -- `Utils.dbg` ("gridSetPixel index: " ++ show index  ++ " v:" ++ show value ++ " "  ++ show (x, y) ++ " for " ++ show (w, h, VU.length g)) where
  index = x + y * w

gridFromImage :: P.ImageData -> GridBase Char
gridFromImage image =
  trace ("gridFromImage " ++ show (width, height, bpp, BS.length bs)) $
    decodeImageData width height bpp bs
 where
  width = fromIntegral $ image ^. (P.size . P.x)
  height = fromIntegral $ image ^. (P.size . P.y)
  bpp = fromIntegral $ image ^. P.bitsPerPixel
  bs = image ^. P.data' :: BS.ByteString

  decodeImageData :: Int -> Int -> Int -> BS.ByteString -> GridBase Char
  decodeImageData idw idh idbpp bytes
    | idbpp == 8 = (idw, idh, VU.fromList $ map (\w -> if w == 0 then '#' else ' ') (BS.unpack bytes))
    | idbpp == 1 = (idw, idh, (\b -> if b then ' ' else '#') `VU.map` unpackBits bytes)
    | otherwise = error "Not implemented"

  -- Unpack bits from ByteString into Vector Bool
  unpackBits :: BS.ByteString -> VU.Vector Bool
  unpackBits ubbs = VU.concatMap unpackByte (VU.fromList $ BS.unpack ubbs)
   where
    unpackByte :: Word8 -> VU.Vector Bool
    unpackByte byte = VU.generate 8 (\i -> testBit byte (7 - i)) -- MSB first

-- Place a building footprint on the grid if possible at the given placement point
addMark :: GridBase Char -> Footprint -> TilePos -> GridBase Char
addMark grid (Footprint pixels) (cx, cy) =
  foldl' (\accGrid (x, y, mark) -> gridSetPixel accGrid (cx + x, cy + y) mark) grid pixels

removeMark :: GridBase Char -> Footprint -> TilePos -> GridBase Char
removeMark grid (Footprint pixels) (cx, cy) =
  foldl' (\accGrid (x, y, _) -> gridSetPixelForce accGrid (cx + x, cy + y) ' ') grid pixels

gridPlace :: GridBase Char -> UnitTypeId -> TilePos -> GridBase Char
gridPlace g u (cx, cy) =
  -- trace ("gridPlace " ++ show u) $
  foldl' (\accGrid (x, y, mark) -> gridSetPixel accGrid (cx + x, cy + y) mark) g ptrn
 where
  ptrn = pixels (getFootprint u)
