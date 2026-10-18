{-# LANGUAGE GADTs #-}

module Istar where

import Actions
import Conduit
import Conduit (filterC, mapC)
import Control.Monad.Extra (when)
import Control.Monad.Trans.Maybe
import Data.Conduit ((.|))
import Data.Function ((&))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.ProtoLens (defMessage)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector.Unboxed qualified as VU
import Debug.Trace (traceM)
import Lens.Micro ((%~), (.~), (^.))
import Lens.Micro.Type (Lens')
import Observation (obsUnitsC, unitsSelf)
import SC2.Grid
import SC2.Grid (gridFromList)
import SC2.Grid.Algo (RegionId)
import SC2.Grid.Core (Grid)
import SC2.Grid.Core qualified as Grid
import SC2.Ids.Ids
import SC2.Ids.UnitTypeId (UnitTypeId)
import SC2.Proto.Data
import SC2.Proto.Data qualified as Proto
import SC2.Spatial (Spatial (..), distManhattan, distSquaredI, tilePos)
import SC2.TilePos (TilePos)
import SC2.Utils (isArmyUnit, tilesInRadius)
import StepMonad
  ( AsyncStaticInfo (..)
  , HasGrid
  , HasObs
  , MaybeStepMonad
  , StaticInfo (..)
  , StepMonad
  , agentGet
  , agentGrid
  , agentModify
  , agentObs
  , agentStatic
  , command
  , debug
  , debugTexts
  )
import Units (Unit, allianceC, isBuilding, runC, unitIdleC, unitTypeC, unitTypeId)
import Utils (unitIsHarvesting)

import Data.List (maximumBy, minimumBy)
import Data.Ord (comparing)
import EnemiesHeatMap
import Lens.Micro.Extras (view)
import Proto.S2clientprotocol.Debug qualified as DebugProto
import SC2.Grid.Algo (adjacent8)
import VisionDecay
import VisitedDecay
import WaypointScore
  ( ScoutPath (..)
  , Waypoint
  , pathLen
  , pathWaypoints
  , pickWaypoint
  , scoutRoute
  , towardsWaypointScore
  )

data ScoutTask
  = ScoutTaskIdle
  | ScoutTaskMove TilePos
  | ScoutTaskRetreat TilePos
  | ScoutTaskScout RegionId (Set TilePos)
  deriving (Eq, Show)

data ScoutContext = ScoutContext
  { scoutUnit :: Maybe Unit
  , scoutTask :: ScoutTask
  , scVisitedDecay :: VisitedDecay
  , scPath :: ScoutPath
  }
  deriving (Eq, Show)

getJust :: (Applicative m) => Maybe a -> MaybeT m a
getJust = MaybeT . pure

stepScouting :: (HasIstar d, HasObs d, HasGrid d) => MaybeStepMonad d ScoutContext
stepScouting = do
  istar <- lift getIstar
  si <- lift agentStatic
  obs <- lift agentObs
  asi <- getJust $ siAsyncStaticInfo si
  enemyBaseRegionId <- getJust $ listToMaybe $ reverse $ asiRegionPathToEnemy asi
  enemyBaseRegion <- getJust $ HashMap.lookup enemyBaseRegionId $ asiRegions asi
  enemyBaseRegionPos <- getJust $ Set.lookupMin enemyBaseRegion
  let scoutContext = istarScoutContext istar
      units = unitsSelf obs
      findScout scout =
        runConduitPure $
          units .| filterC (\unit -> scout ^. #tag == unit ^. #tag) .| headC
  case scoutTask scoutContext of
    ScoutTaskIdle -> do
      probe <- getJust $ listToMaybe $ runC $ units .| unitTypeC ProtossProbe .| filterC unitIsHarvesting
      waypoints <- lift $ pathWaypoints (tilePos probe) enemyBaseRegionPos
      pure $
        scoutContext
          { scoutUnit = Just probe
          , scoutTask = ScoutTaskMove enemyBaseRegionPos
          , scVisitedDecay = scVisitedDecay scoutContext
          , scPath = waypoints
          }
    ScoutTaskMove destPos -> do
      scout <- getJust $ scoutUnit scoutContext
      unitAlive <- getJust $ findScout scout
      if unitAlive ^. #shield < unitAlive ^. #shieldMax
        then
          pure $
            scoutContext
              { scoutUnit = Just unitAlive
              , scoutTask = ScoutTaskRetreat (startLocation si)
              , scVisitedDecay = scVisitedDecay scoutContext
              }
        else do
          let dist = distManhattan unitAlive destPos
          traceM $ "!! dist to " ++ show dist
          if dist < 20
            then
              pure $
                scoutContext
                  { scoutUnit = Just unitAlive
                  , scoutTask = ScoutTaskRetreat (startLocation si)
                  , scVisitedDecay = scVisitedDecay scoutContext
                  }
            else
              pure $
                scoutContext
                  { scoutUnit = Just unitAlive
                  , scVisitedDecay = scVisitedDecay scoutContext
                  }
    ScoutTaskRetreat destPos -> do
      scout <- getJust $ scoutUnit scoutContext
      unitAlive <- getJust $ findScout scout
      if distManhattan unitAlive destPos < 20
        then
          pure $
            scoutContext
              { scoutUnit = Just unitAlive
              , scoutTask = ScoutTaskMove enemyBaseRegionPos
              , scVisitedDecay = scVisitedDecay scoutContext
              }
        else
          pure $
            scoutContext
              { scoutUnit = Just unitAlive
              , scoutTask = ScoutTaskRetreat destPos
              , scVisitedDecay = scVisitedDecay scoutContext
              }
    _ -> pure scoutContext

data IstarState = IstarState
  { istarSeenEnemies :: Set UnitTypeId
  , istarSeenBuildings :: Set UnitTypeId
  , istarVisionDecay :: VisionDecay
  , istarScoutContext :: ScoutContext
  , istarGroundHeatMap :: HeatMap
  }

class HasIstar d where
  scoutingL :: Lens' d IstarState

getIstar :: (HasIstar d) => StepMonad d IstarState
getIstar = (^. scoutingL) <$> agentGet

tileCenter :: TilePos -> Point2D
tileCenter (tx, ty) =
  defMessage & #x .~ fromIntegral tx + 0.5 & #y .~ fromIntegral ty + 0.5

tilesCandidates :: (HasGrid d) => Unit -> StepMonad d [TilePos]
tilesCandidates u = do
  traitsMap <- unitTraits <$> agentStatic
  grid <- agentGrid
  let traits = traitsMap HashMap.! Units.unitTypeId u
      visionRadius = traits ^. #sightRange :: Float
      -- tiles = [t | t <- tilesInRadius (floor visionRadius) (tilePos u), gridPixel grid t /= '#']
      tiles = [t | t <- adjacent8 (tilePos u), gridPixel grid t == ' ' || gridPixel grid t == '/']
  pure tiles

destinationProgress :: TilePos -> TilePos -> TilePos -> Float
destinationProgress current candidate dest =
  sqrt (fromIntegral $ distSquaredI current dest)
    - sqrt (fromIntegral $ distSquaredI candidate dest)

debugScoutPath :: ScoutPath -> StepMonad d ()
debugScoutPath path = do
  heights <- heightMap <$> agentStatic
  let route = scoutRoute path
      waypoints = scoutWaypoints path
      pathColor = defMessage & #r .~ 0 & #g .~ 255 & #b .~ 255
      waypointColor = defMessage & #r .~ 255 & #g .~ 180 & #b .~ 0
      pathLines =
        [ (pathColor, lineBetween (tilePoint heights from) (tilePoint heights to))
        | (from, to) <- zip route (drop 1 route)
        ]
      waypointLines =
        concat
          [ let center = tileCenter pos
                x = center ^. #x
                y = center ^. #y
                z = tileHeight heights pos + 2
                left = addZ z (defMessage & #x .~ x - 0.2 & #y .~ y)
                right = addZ z (defMessage & #x .~ x + 0.2 & #y .~ y)
                bottom = addZ z (defMessage & #x .~ x & #y .~ y - 0.2)
                top = addZ z (defMessage & #x .~ x & #y .~ y + 0.2)
             in [ (waypointColor, lineBetween left right)
                , (waypointColor, lineBetween bottom top)
                ]
          | (pos, _) <- waypoints
          ]
      waypointLabels =
        [ (show index ++ ": " ++ show pos, addZ (tileHeight heights pos + 3) (tileCenter pos))
        | (pos, index) <- waypoints
        ]
  when (not (null pathLines && null waypointLines)) $
    StepMonad.debug [DebugLine (pathLines ++ waypointLines)]
  debugTexts waypointLabels
 where
  tileHeight heights pos = fromIntegral (fromEnum (gridPixel heights pos))
  tilePoint heights pos = addZ (tileHeight heights pos + 1) (tileCenter pos)
  lineBetween :: Point -> Point -> DebugProto.Line
  lineBetween p0 p1 = defMessage & #p0 .~ p0 & #p1 .~ p1

addZ :: Float -> Point2D -> Point
addZ pz p = defMessage & #x .~ (p ^. #x) & #y .~ (p ^. #y) & #z .~ pz

beamObstacleLen :: (HasGrid d) => Unit -> TilePos -> StepMonad d Int
beamObstacleLen u end = do
  grid <- agentGrid
  let start = tilePos u
      z = u ^. (#pos . #z)
      ray = obstacleRay grid start end
      rayEnd = fromMaybe start (listToMaybe ray)
      line = defMessage & #p0 .~ (addZ z (tileCenter start)) & #p1 .~ (addZ z (tileCenter rayEnd))
      colorGreen = defMessage & #r .~ 0 & #g .~ 1 & #b .~ 0
  StepMonad.debug [DebugLine [(colorGreen, line)]]

  pure $ length ray

obstacleRay :: Grid -> TilePos -> TilePos -> [TilePos]
obstacleRay grid start end =
  fromMaybe [] $ gridRaycastTile grid start (end - start)

openPathScore :: Grid -> TilePos -> TilePos -> Float
openPathScore grid start end =
  scoreOpenPathLength . length $ obstacleRay grid start end

rampScore :: Float -> Float -> Float -> Float
rampScore value low high =
  clamp01 ((value - low) / (high - low))

scoreOpenPathLength :: Int -> Float
scoreOpenPathLength rayLength =
  clamp01 $ rampScore (fromIntegral rayLength) 3 10

calcAgeScore :: (HasIstar d) => Unit -> TilePos -> StepMonad d Float
calcAgeScore u candidate = do
  traitsMap <- unitTraits <$> agentStatic
  vision <- istarVisionDecay . view scoutingL <$> agentGet
  let tiles = tilesInRadius (floor visionRadius) candidate
      traits = traitsMap HashMap.! Units.unitTypeId u
      visionRadius = traits ^. #sightRange :: Float
  pure $ ageScore [visionDecayTile vision tpos | tpos <- tiles]

ageScore :: [FogTile] -> Float
ageScore tiles = average $ scoreTile <$> tiles
 where
  scoreTile tile =
    case fogTileState tile of
      FogTileUnknown -> 1
      FogTileBlocked -> 0
      _ -> saturatingScore (fromIntegral $ fogTileAge tile) 500

  average [] = 0
  average values = sum values / fromIntegral (length values)

saturatingScore :: Float -> Float -> Float
saturatingScore value scale =
  let nonNegativeValue = max 0 value
   in nonNegativeValue / (nonNegativeValue + scale)

clamp01 :: Float -> Float
clamp01 = max 0 . min 1

progressScore :: TilePos -> TilePos -> TilePos -> Float
progressScore current candidate dest =
  clamp01
    ( 0.5
        + destinationProgress current candidate dest / (2 * sqrt 2)
    )

safetyScore :: HeatMap -> TilePos -> Float
safetyScore enemiesHeatMap candidate =
  1 - saturatingScore candidateThreat 20
 where
  candidateThreat =
    maximum
      ( 0
          : [ gridPixel enemiesHeatMap t
            | t <- tilesInRadius 3 candidate
            ]
      )

scoutScorePos ::
  (HasGrid d) =>
  Unit ->
  TilePos ->
  TilePos ->
  [Waypoint] ->
  StepMonad d Float
scoutScorePos u candidate dest wps = do
  grid <- agentGrid

  rayLength <- beamObstacleLen u candidate
  waypoint <- pickWaypoint (tilePos u) wps
  -- debug
  let currentDist = pathLen grid (tilePos u) (fst waypoint)

  traceM $
    "picked WP="
      ++ show waypoint
      ++ " currentDist="
      ++ show currentDist

  let d = pathLen grid candidate (fst waypoint)
  traceM $
    "candidate="
      ++ show candidate
      ++ " dist="
      ++ show d
      ++ " progress="
      ++ show (currentDist - d)
  -- debug

  -- Candidates are adjacent tiles, so their distance change is at most
  -- sqrt 2. Map that progress from [-sqrt 2, sqrt 2] into [0, 1].
  let currentProgressScore = progressScore (tilePos u) candidate dest
      candidateOpenPathScore = scoreOpenPathLength rayLength

      waypointScore = towardsWaypointScore grid (tilePos u) candidate waypoint
  -- pure . clamp01 $ candidateOpenPathScore + currentProgressScore + waypointScore
  -- pure $ candidateOpenPathScore + (waypointScore)
  traceM $ "waypoint " ++ show waypoint
  -- pure $ candidateOpenPathScore * waypointScore
  pure $ waypointScore

-- 0.8 * waypointScore
--   + 0.2 * openPathScore

-- 0.35 * waypointScore
--   + 0.20 * progressScore
--   + 0.30 * openPathScore
--   + 0.10 * ageScore
--   + 0.05 * progressScore

-- pure . clamp01 $
--   0.10 * ageScore
--     + 0.20 * progressScore
--     + 0.35 * waypointScore
--     + 0.15 * safetyScore
--     + 0.15 * (1 - clamp01 visitedScore)

commandScouting :: (HasObs d, HasGrid d) => Maybe ScoutContext -> StepMonad d ()
commandScouting Nothing = pure ()
commandScouting (Just (ScoutContext (Just u) (ScoutTaskMove dest) _ path)) = do
  debugScoutPath path
  candidates <- tilesCandidates u
  grid <- agentGrid
  traceM $ "candidates: " ++ show candidates
  scored <-
    mapM
      ( \candidate -> do
          score <- scoutScorePos u candidate dest (scoutWaypoints path)
          pure (candidate, score)
      )
      candidates
  traceM $ "scored candidates: " ++ show scored

  let res@(dest, _) = maximumBy (comparing snd) scored
  traceM $ "!! scoring winner: " ++ show res
  command [PointCommand MOVE [u] (tileCenter dest)]
commandScouting (Just (ScoutContext (Just u) (ScoutTaskRetreat dest) _ _)) = command [PointCommand MOVE [u] (toPoint2D dest)]
commandScouting _ = pure ()

modifyIstar ::
  (HasIstar d) =>
  (IstarState -> IstarState) ->
  StepMonad d ()
modifyIstar f = agentModify (scoutingL %~ f)

istarEmpty :: Grid -> IstarState
istarEmpty grid =
  IstarState
    Set.empty
    Set.empty
    (visionDecayEmpty grid)
    (ScoutContext Nothing ScoutTaskIdle (visitedDecayEmpty grid) (ScoutPath [] []))
    (gridFromList (gridW grid) (gridH grid) (replicate (gridW grid * gridH grid) 0))

stepIstar :: (StepMonad.HasObs d, HasGrid d, HasIstar d, HasEnemiesHeatMap d) => StepMonad d ()
stepIstar = do
  obs <- agentObs
  let enemies =
        Set.fromList $
          runC $
            obsUnitsC obs .| allianceC Enemy .| filterC isArmyUnit .| mapC Units.unitTypeId
      enemyBuildings =
        Set.fromList $
          runC $
            obsUnitsC obs .| allianceC Enemy .| filterC isBuilding .| mapC Units.unitTypeId
  updateHeatMap
  debugHeatMap
  debugVisionDecay
  scoutState' <- runMaybeT stepScouting
  commandScouting scoutState'
  modifyIstar $ \current ->
    let scoutContext = fromMaybe (istarScoutContext current) scoutState'
        agedVisits = visitedDecayStep (scVisitedDecay scoutContext)
        visited =
          maybe agedVisits (\unit -> visitedDecayVisit (tilePos unit) agedVisits) $
            scoutState' >>= scoutUnit
     in current
          { istarSeenEnemies = istarSeenEnemies current `Set.union` enemies
          , istarSeenBuildings = istarSeenBuildings current `Set.union` enemyBuildings
          , istarScoutContext = scoutContext{scVisitedDecay = visited}
          , istarVisionDecay = stepVisionDecay obs (istarVisionDecay current)
          }

debugVisionDecay :: (HasIstar d) => StepMonad d ()
debugVisionDecay = do
  vision <- istarVisionDecay . view scoutingL <$> agentGet
  heights <- heightMap <$> agentStatic

  debugTexts
    [ (show visionUnit, point3D (fromIntegral x) (fromIntegral y) (fromIntegral z + 10))
    | x <- [0 .. vdWidth vision - 1]
    , y <- [0 .. vdHeight vision - 1]
    , FogTile age (FogTileUnit u) <- [visionDecayTile vision (x, y)]
    , let visionUnit = (u ^. #tag, age)
    , let z = fromEnum $ gridPixel heights (x, y)
    -- , danger > 0
    ]
 where
  point3D x y z = defMessage & #x .~ x & #y .~ y & #z .~ z :: Point
