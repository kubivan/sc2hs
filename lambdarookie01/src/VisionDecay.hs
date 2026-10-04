{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}

module VisionDecay where

import Actions (UnitTag)
import Conduit (filterC, (.|))
import Data.Bits
import Data.ByteString qualified as BS
import Data.Conduit.Combinators (sinkList)
import Data.ProtoLens (defMessage)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Debug.Trace (trace)
import GHC.Word (Word64, Word8)
import Lens.Micro
import Observation (Observation, obsUnitsC)
import Proto.S2clientprotocol.Common qualified as P
import Proto.S2clientprotocol.Common_Fields qualified as P
import Proto.S2clientprotocol.Raw (Alliance (Enemy))
import SC2.Grid qualified as Grid
import SC2.Grid.Core
import SC2.Proto.Data (ImageData)
import SC2.Spatial (tilePos)
import SC2.TilePos (TilePos)
import SC2.Utils (isArmyUnit)
import StepMonad (HasObs, StepMonad, agentObs)
import Units (Unit, allianceC, runC)

data FogTileState
  = FogTileUnknown
  | FogTileClear
  | FogTileUnit Unit
  | FogTileBlocked
  deriving (Eq, Show)

data FogTile = FogTile
  { fogTileAge :: !Word64
  , fogTileState :: !FogTileState
  }
  deriving (Eq, Show)

data VisionDecay = VisionDecay
  { vdWidth :: !Int
  , vdHeight :: !Int
  , vdTiles :: !(Vector FogTile)
  }
  deriving (Eq, Show)

visionDecayEmpty :: Grid -> VisionDecay
visionDecayEmpty grid =
  VisionDecay
    { vdWidth = w
    , vdHeight = h
    , vdTiles = Vector.generate (w * h) $ \i ->
        initialTile (indexToPos w i)
    }
 where
  w = gridW grid
  h = gridH grid

  initialTile pos =
    FogTile
      { fogTileAge = 0
      , fogTileState =
          case grid Grid.! pos of
            ' ' -> FogTileUnknown
            '/' -> FogTileUnknown
            '#' -> FogTileBlocked
            c -> error ("!!! assert: invalid grid pixel: " ++ show c)
      }

indexToPos :: Int -> Int -> TilePos
indexToPos width i =
  (i `mod` width, i `div` width)

visionDecayIndex :: VisionDecay -> TilePos -> Int
visionDecayIndex VisionDecay{vdWidth} (x, y) =
  y * vdWidth + x

visionDecayTile :: VisionDecay -> TilePos -> FogTile
visionDecayTile vd@VisionDecay{vdTiles} pos =
  vdTiles Vector.! visionDecayIndex vd pos

visionDecayStep :: VisionDecay -> VisionDecay
visionDecayStep vision =
  vision
    { vdTiles =
        Vector.map bumpAge (vdTiles vision)
    }
 where
  bumpAge tile =
    case fogTileState tile of
      FogTileUnknown -> tile
      FogTileBlocked -> tile
      _ ->
        tile
          { fogTileAge = fogTileAge tile + 1
          }

findPreviousUnit :: VisionDecay -> UnitTag -> Maybe TilePos
findPreviousUnit vision tag =
  indexToPos (vdWidth vision) <$> Vector.findIndex isUnit (vdTiles vision)
 where
  isUnit tile =
    case fogTileState tile of
      FogTileUnit unit -> unit ^. #tag == tag
      _ -> False

imageDataByteAt :: P.ImageData -> TilePos -> Word8
imageDataByteAt image (x, y) =
  BS.index bytes (y * width + x)
 where
  width = fromIntegral $ image ^. (P.size . P.x)
  bytes = image ^. P.data'

visibilityAt :: P.ImageData -> TilePos -> Word8
visibilityAt image pos =
  case fromIntegral (image ^. P.bitsPerPixel) of
    8 ->
      imageDataByteAt image pos
    1 ->
      let byte = imageDataByteAt image (x `div` 8, y)
          bit = 7 - x `mod` 8
       in fromIntegral . fromEnum $ testBit byte bit
    bpp ->
      error $ "Unsupported visibility bits per pixel: " ++ show bpp
 where
  (x, y) = pos

visionDecayClearVisible :: P.ImageData -> VisionDecay -> VisionDecay
visionDecayClearVisible visibility vision =
  vision
    { vdTiles =
        Vector.imap clearVisible (vdTiles vision)
    }
 where
  -- optional ImageData visibility_map = 2;
  -- // uint8. 0=Hidden, 1=Fogged, 2=Visible, 3=FullHidden

  clearVisible i tile
    | visibilityAt visibility (indexToPos (vdWidth vision) i) == 2 =
        tile
          { fogTileAge = 0
          , fogTileState =
              case fogTileState tile of
                FogTileBlocked -> FogTileBlocked
                _ -> FogTileClear
          }
    | otherwise =
        tile

visionDecayAddEnemy :: VisionDecay -> Unit -> VisionDecay
visionDecayAddEnemy vision unit =
  trace
    ("!!! add enemy to vision " ++ show unit)
    vision
      { vdTiles =
          vdTiles vision
            Vector.// updates
      }
 where
  newPos = tilePos unit

  oldPos = findPreviousUnit vision (unit ^. #tag)

  -- remove old unit if found ++ set new(?) enemy pos
  updates =
    [ (visionDecayIndex vision oldPos, unknownTile)
    | Just oldPos <- [oldPos]
    , oldPos /= newPos
    ]
      ++ [ (visionDecayIndex vision newPos, enemyTile)
         ]

  unknownTile =
    FogTile
      { fogTileAge = 0
      , fogTileState = FogTileUnknown
      }

  enemyTile =
    FogTile
      { fogTileAge = 0
      , fogTileState = FogTileUnit unit
      }

stepVisionDecay :: Observation -> VisionDecay -> VisionDecay
stepVisionDecay obs vision =
  vision
    & visionDecayStep
    & visionDecayClearVisible (obs ^. (#rawData . #mapState . #visibility))
    & addEnemies enemyUnits
 where
  enemyUnits =
    runC $
      obsUnitsC obs
        .| allianceC Enemy
  -- .| filterC isArmyUnit
  addEnemies :: [Unit] -> VisionDecay -> VisionDecay
  addEnemies enemies v =
    foldl' visionDecayAddEnemy v enemies
