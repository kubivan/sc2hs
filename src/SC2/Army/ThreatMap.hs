{-# LANGUAGE OverloadedLabels #-}
module SC2.Army.ThreatMap (
    ThreatMap(..)
  , ThreatGrid
  , computeThreatMap
  , regionThreat
) where

import StepMonad (StepMonad, agentStatic, agentObs, AgentDynamicState, StaticInfo(..))
import Observation (obsUnitsC)
import Units (runC)
import SC2.Grid (Grid, gridW, gridH, tilePos, RegionId)
import SC2.Units.CombatEval (unitDps, unitRange)
import SC2.Utils (isEnemy, tilesInRadius)

import Conduit (filterC, (.|))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as VU
import Lens.Micro ((^.))

import Proto.S2clientprotocol.Raw (Unit)
import Control.Monad (foldM)

-- | Grid of continuous threat values (enemy DPS influence after falloff).
-- Stored separately from terrain grid (which is Char based).
-- (width, height, vector row-major)

type ThreatGrid = (Int, Int, Vector Float)

data ThreatMap = ThreatMap {
    tmGrid :: ThreatGrid
  , tmRegionThreat :: HashMap RegionId Float
} deriving (Show)

-- | Aggregate threat for a region (0 if absent).
regionThreat :: ThreatMap -> RegionId -> Float
regionThreat (ThreatMap _ rmap) rid = HashMap.lookupDefault 0 rid rmap

-- | Compute enemy threat map: each enemy unit contributes its DPS
-- linearly decreasing to 0 at its weapon range.
computeThreatMap :: AgentDynamicState d => StepMonad d ThreatMap
computeThreatMap = do
  si <- agentStatic
  obs <- agentObs
  let heightMapGrid = heightMap si
      w = gridW heightMapGrid
      h = gridH heightMapGrid
      baseGrid = VU.replicate (w * h) 0.0 :: Vector Float
      enemies = runC $ obsUnitsC obs .| filterC isEnemy
  gridFilled <- foldM (\g u -> do
                           dps <- unitDps u
                           range <- unitRange u
                           pure (applyThreat w h u dps range g)) baseGrid enemies
  let regionThreats = aggregateRegions w h (regionLookup si) gridFilled
  pure $ ThreatMap (w, h, gridFilled) regionThreats

-- Apply a single unit's threat into the grid vector.
applyThreat :: Int -> Int -> Unit -> Float -> Float -> Vector Float -> Vector Float
applyThreat w h u dps range vec =
  if range <= 0 || dps <= 0 then vec else
    let center = tilePos (u ^. #pos)
        r = ceiling range
        tiles = filter inBounds $ tilesInRadius r center
        inBounds (x,y) = x >= 0 && x < w && y >= 0 && y < h
        addOne acc (tx, ty) =
          let dx = tx - fst center
              dy = ty - snd center
              dist = sqrt (fromIntegral (dx*dx + dy*dy))
              wFactor = max 0 (1 - dist / range)
              contrib = dps * wFactor
              idx = tx + ty * w
              old = acc VU.! idx
          in acc VU.// [(idx, old + contrib)]
    in foldl addOne vec tiles

-- Build region threat aggregation from grid.
aggregateRegions :: Int -> Int -> HashMap (Int, Int) RegionId -> Vector Float -> HashMap RegionId Float
aggregateRegions w h lookupMap vec =
  let go idx acc =
        let x = idx `mod` w
            y = idx `div` w
            val = vec VU.! idx
        in if val <= 0 then acc else
             case HashMap.lookup (x,y) lookupMap of
               Nothing -> acc
               Just rid -> HashMap.insertWith (+) rid val acc
  in foldr go HashMap.empty [0 .. w*h - 1]
