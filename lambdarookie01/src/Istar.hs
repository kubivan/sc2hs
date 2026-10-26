{-# LANGUAGE GADTs #-}

module Istar where

import Actions
import Conduit
import Control.Monad.Extra (when)
import Control.Monad.Trans.Maybe
import Data.Function ((&))
import Data.HashMap.Strict qualified as HashMap
import Data.Maybe (fromMaybe, listToMaybe)
import Data.ProtoLens (defMessage)
import Data.Set (Set)
import Data.Set qualified as Set
import Debug.Trace (traceM)
import Lens.Micro ((%~), (.~), (^.))
import Lens.Micro.Type (Lens')
import Observation (obsUnitsC, unitsSelf)
import SC2.Grid
import SC2.Ids.Ids
import SC2.Proto.Data
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
import Units (allianceC, isBuilding, runC, unitTypeC, unitTypeId)
import Utils (unitIsHarvesting)

import ArmyHeatMap
import Data.List (maximumBy)
import Data.Ord (comparing)
import Fmt (fmt, (+||), (||+))
import Lens.Micro.Extras (view)
import Proto.S2clientprotocol.Debug qualified as DebugProto
import ScoringUtils (clamp01, saturatingScore)
import VisionDecay
import VisitedDecay
import WaypointScore
  ( ScoutPath (..)
  , Waypoint
  , calcAlongPathScore
  , pathWaypoints
  , scoutRoute
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
          traceM . fmt $ "!! dist to " +|| dist ||+ ""
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
  , istarGroundSelfHeatMap :: HeatMap
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
        [ (fmt $ "" +|| index ||+ ": " +|| pos ||+ "", addZ (tileHeight heights pos + 3) (tileCenter pos))
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
  (HasGrid d, HasIstar d, HasObs d) =>
  Unit ->
  TilePos ->
  TilePos ->
  [Waypoint] ->
  VisitedDecay ->
  VisionDecay ->
  StepMonad d Float
scoutScorePos u candidate dest wps visited vision = do
  rayLength <- beamObstacleLen u candidate
  ageScore <- calcAgeScore vision u candidate
  visitedScore <- calcVisitedDecayScore candidate visited

  alongPathScore <- calcAlongPathScore wps u candidate
  -- debug
  let -- Candidates are adjacent tiles, so their distance change is at most
      -- sqrt 2. Map that progress from [-sqrt 2, sqrt 2] into [0, 1].
      currentProgressScore = progressScore (tilePos u) candidate dest
      candidateOpenPathScore = scoreOpenPathLength rayLength

  -- debug
  traceM . fmt $
    "candidate="
      +|| candidate
      ||+ " openPathScore: "
      +|| candidateOpenPathScore
      ||+ " visitedScore: "
      +|| visitedScore
      ||+ ""

  -- pure . clamp01 $ candidateOpenPathScore + currentProgressScore + waypointScore
  -- pure $ candidateOpenPathScore + (waypointScore)
  -- pure $ candidateOpenPathScore * waypointScore
  pure $ -- candidateOpenPathScore + waypointScore
    0.1 * alongPathScore
      + 0.2 * candidateOpenPathScore
      + 0.3 * visitedScore
      + 0.4 * ageScore

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

commandScouting :: (HasObs d, HasGrid d, HasIstar d) => Maybe ScoutContext -> StepMonad d ()
commandScouting Nothing = pure ()
commandScouting (Just (ScoutContext (Just u) (ScoutTaskMove dest) visited path)) = do
  debugScoutPath path
  candidates <- tilesCandidates u
  vision <- istarVisionDecay . view scoutingL <$> agentGet
  traceM . fmt $ "candidates: " +|| candidates ||+ ""
  scored <-
    mapM
      ( \candidate -> do
          score <- scoutScorePos u candidate dest (scoutWaypoints path) visited vision
          pure (candidate, score)
      )
      candidates
  traceM . fmt $ "scored candidates: " +|| scored ||+ ""

  let res@(dest, _) = maximumBy (comparing snd) scored
  traceM . fmt $ "!! scoring winner: " +|| res ||+ ""
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
    (gridFromList (gridW grid) (gridH grid) (replicate (gridW grid * gridH grid) 0))

stepIstar :: (StepMonad.HasObs d, HasGrid d, HasIstar d, HasArmyHeatMap d) => StepMonad d ()
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
  let frame = obs ^. #gameLoop
  updateHeatMap
  debugHeatMaps
  debugVisionDecay
  scoutState' <- runMaybeT stepScouting
  commandScouting scoutState'
  modifyIstar $ \current ->
    let scoutContext = fromMaybe (istarScoutContext current) scoutState'
        visited =
          maybe
            (scVisitedDecay scoutContext)
            (\unit -> visitedDecayVisit frame (tilePos unit) (scVisitedDecay scoutContext))
            $ scoutState' >>= scoutUnit
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
