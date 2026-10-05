{-# LANGUAGE RankNTypes #-}

module EnemiesHeatMap (HeatMap, updateHeatMap, HasEnemiesHeatMap (..), debugHeatMap) where

import Actions (UnitTag)
import Conduit
import Data.Vector.Unboxed qualified as VU
import Lens.Micro
import Observation (obsUnitsC)
import SC2.Grid
import SC2.Proto.Data
import SC2.Spatial (tilePos)
import SC2.Utils (isArmyUnit)
import StepMonad
import Units

import Control.Monad (forM_, when)
import Control.Monad.ST (ST)
import Data.HashMap.Strict qualified as HashMap
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.ProtoLens (defMessage)
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import SC2.TilePos (TilePos)

type HeatMap = GridBase Float

class HasEnemiesHeatMap d where
  heatGroundEnemiesL :: Lens' d HeatMap

buildGrid ::
  (VU.Unbox a) =>
  Int ->
  Int ->
  a ->
  (forall s. VUM.MVector s a -> ST s ()) ->
  GridBase a
buildGrid w h initial fill =
  ( w
  , h
  , VU.create $ do
      v <- VUM.replicate (w * h) initial
      fill v
      pure v
  )

addToGrid ::
  (VU.Unbox a, Num a) =>
  Int ->
  Int ->
  Int ->
  a ->
  VUM.MVector s a ->
  ST s ()
addToGrid w x y value v = do
  let i = y * w + x
  -- old <- VUM.read v i
  -- old <- fromMaybe 0 <$> VUM.readMaybe v i
  old <- VUM.readMaybe v i
  case old of
    Just old -> VUM.write v i (old + value)
    Nothing -> pure ()

isGroundWeapon :: Weapon -> Bool
isGroundWeapon weapon =
  case weapon ^. #type' of
    Weapon'Ground -> True
    Weapon'Any -> True
    Weapon'Air -> False

groundWeapon :: UnitTypeData -> Maybe Weapon
groundWeapon udata = find isGroundWeapon (udata ^. #weapons)

unitAttack :: UnitTraits -> Unit -> Float
unitAttack traits u =
  case traits HashMap.!? Units.unitTypeId u of
    Just udata ->
      case groundWeapon udata of
        Just weapon -> (weapon ^. #damage) * fromIntegral (weapon ^. #attacks)
        Nothing -> 0
    Nothing -> 0

unitAttackRange :: UnitTraits -> Unit -> Int
unitAttackRange traits u =
  case traits HashMap.!? Units.unitTypeId u of
    Just udata ->
      case groundWeapon udata of
        Just weapon -> ceiling (weapon ^. #range)
        Nothing -> 0
    Nothing -> 0

enemyHeatMap :: UnitTraits -> Int -> Int -> [Unit] -> GridBase Float
enemyHeatMap utraits w h enemies =
  buildGrid w h 0 $ \grid -> do
    forM_ enemies $ \enemy -> do
      let (ux, uy) = tilePos enemy
          range = unitAttackRange utraits enemy
          damage = unitAttack utraits enemy
          x0 = max 0 (ux - range)
          x1 = min (w - 1) (ux + range)
          y0 = max 0 (uy - range)
          y1 = min (h - 1) (uy + range)
      forM_ [(x, y) | y <- [y0 .. y1], x <- [x0 .. x1]] $ \(x, y) -> do
        let dx = x - ux
            dy = y - uy
        when (dx * dx + dy * dy <= range * range) $
          addToGrid w x y damage grid

debugHeatMap :: (HasEnemiesHeatMap d) => StepMonad d ()
debugHeatMap = do
  heights <- heightMap <$> agentStatic
  heatMap <- (^. heatGroundEnemiesL) <$> agentGet

  debugTexts
    [ (show danger, point3D (fromIntegral x) (fromIntegral y) (fromIntegral z + 10))
    | x <- [0 .. gridW heatMap - 1]
    , y <- [0 .. gridH heatMap - 1]
    , let danger = gridPixel heatMap (x, y)
    , let z = fromEnum $ gridPixel heights (x, y)
    , danger > 0
    ]
 where
  point3D x y z = defMessage & #x .~ x & #y .~ y & #z .~ z :: Point

updateHeatMap :: (HasObs d, HasGrid d, HasEnemiesHeatMap d) => StepMonad d ()
updateHeatMap = do
  obs <- agentObs
  g <- agentGrid
  si <- agentStatic
  let enemies =
        runC $
          obsUnitsC obs
            .| allianceC Enemy
            .| filterC isArmyUnit
      w = gridW g
      h = gridH g
      heatMap = enemyHeatMap (unitTraits si) w h enemies
  agentModify (heatGroundEnemiesL .~ heatMap)
  pure ()
