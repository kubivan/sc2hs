{-# LANGUAGE GADTs #-}

module Istar where

import Actions
import Conduit
import Conduit (filterC, mapC)
import Control.Monad.Extra (when)
import Control.Monad.Trans.Maybe
import Data.Conduit ((.|))
import Data.Foldable (find)
import Data.Function ((&))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.ProtoLens (defMessage)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector.Unboxed qualified as VU
import Data.Word
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

import Data.List (maximumBy)
import Data.Ord (comparing)
import EnemiesHeatMap
import Lens.Micro.Extras (view)
import SC2.Grid.Algo (adjacent8)
import VisionDecay
import VisitedDecay

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
  }
  deriving (Eq, Show)

getJust :: (Applicative m) => Maybe a -> MaybeT m a
getJust = MaybeT . pure

stepScouting :: (HasIstar d, HasObs d) => MaybeStepMonad d ScoutContext
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
  traceM $ "IstarScouting: " ++ show scoutContext
  case scoutTask scoutContext of
    ScoutTaskIdle -> do
      probe <- getJust $ listToMaybe $ runC $ units .| unitTypeC ProtossProbe .| filterC unitIsHarvesting
      pure $
        ScoutContext
          { scoutUnit = Just probe
          , scoutTask = ScoutTaskMove enemyBaseRegionPos
          , scVisitedDecay = scVisitedDecay scoutContext
          }
    ScoutTaskMove destPos -> do
      scout <- getJust $ scoutUnit scoutContext
      unitAlive <- getJust $ findScout scout
      if unitAlive ^. #shield < unitAlive ^. #shieldMax
        then
          pure $
            ScoutContext
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
                ScoutContext
                  { scoutUnit = Just unitAlive
                  , scoutTask = ScoutTaskRetreat (startLocation si)
                  , scVisitedDecay = scVisitedDecay scoutContext
                  }
            else
              pure $
                ScoutContext
                  { scoutUnit = Just unitAlive
                  , scoutTask = ScoutTaskMove destPos
                  , scVisitedDecay = scVisitedDecay scoutContext
                  }
    ScoutTaskRetreat destPos -> do
      scout <- getJust $ scoutUnit scoutContext
      unitAlive <- getJust $ findScout scout
      if distManhattan unitAlive destPos < 20
        then
          pure $
            ScoutContext
              { scoutUnit = Just unitAlive
              , scoutTask = ScoutTaskMove enemyBaseRegionPos
              , scVisitedDecay = scVisitedDecay scoutContext
              }
        else
          pure $
            ScoutContext
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
      tiles = [t | t <- adjacent8 (tilePos u), gridPixel grid t /= '#']
  pure tiles

destinationProgress :: TilePos -> TilePos -> TilePos -> Float
destinationProgress current candidate dest =
  fromIntegral $
    distSquaredI current dest - distSquaredI candidate dest

addZ :: Float -> Point2D -> Point
addZ pz p = defMessage & #x .~ (p ^. #x) & #y .~ (p ^. #y) & #z .~ pz

beamObstacleLen :: (HasGrid d) => Unit -> TilePos -> StepMonad d Int
beamObstacleLen u end = do
  grid <- agentGrid
  let start = tilePos u
      z = u ^. (#pos . #z)
      direction = end - start
      ray = fromMaybe [] $ gridRaycastTile grid start direction
      rayEnd = fromMaybe start (listToMaybe ray)
      line = defMessage & #p0 .~ (addZ z (tileCenter start)) & #p1 .~ (addZ z (tileCenter rayEnd))
      colorGreen = defMessage & #r .~ 0 & #g .~ 1 & #b .~ 0
  StepMonad.debug [DebugLine [(colorGreen, line)]]

  pure $ length ray

scoutScorePos ::
  (HasIstar d, HasGrid d) =>
  Unit ->
  TilePos ->
  TilePos ->
  VisitedDecay ->
  StepMonad d Float
scoutScorePos u candidate dest visited = do
  traitsMap <- unitTraits <$> agentStatic
  vision <- istarVisionDecay . view scoutingL <$> agentGet
  enemiesHeatMap <- istarGroundHeatMap . view scoutingL <$> agentGet

  deadEndScore <- fromIntegral <$> beamObstacleLen u candidate

  let traits = traitsMap HashMap.! Units.unitTypeId u
      visionRadius = traits ^. #sightRange :: Float
      tiles = tilesInRadius (floor visionRadius) candidate

      ageScore =
        foldl'
          ( \s tpos ->
              s + fromIntegral (tileAge (visionDecayTile vision tpos))
          )
          0
          tiles

      progressScore = destinationProgress (tilePos u) candidate dest
      candidateThreat =
        maximum
          [ gridPixel enemiesHeatMap t
          | t <- tilesInRadius 3 candidate
          ]

      visitedScore = visitedDecayScore visited candidate
  -- distance (tilePos u) dest
  --   - distance candidate dest

  pure $
    ageScore
      + 10 * progressScore
      - 20 * candidateThreat
      - 100 * visitedScore
      + 50 * deadEndScore

commandScouting :: (HasObs d, HasIstar d, HasGrid d) => Maybe ScoutContext -> StepMonad d ()
commandScouting Nothing = pure ()
commandScouting (Just (ScoutContext (Just u) (ScoutTaskMove dest) visited)) = do
  candidates <- tilesCandidates u
  scored <-
    mapM
      ( \candidate -> do
          score <- scoutScorePos u candidate dest visited
          pure (candidate, score)
      )
      candidates

  let (dest, _) = maximumBy (comparing snd) scored
  command [PointCommand MOVE [u] (tileCenter dest)]
commandScouting (Just (ScoutContext (Just u) (ScoutTaskRetreat dest) _)) = command [PointCommand MOVE [u] (toPoint2D dest)]
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
    (ScoutContext Nothing ScoutTaskIdle (visitedDecayEmpty grid))
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

tileAge :: FogTile -> Word64
tileAge tile =
  case fogTileState tile of
    FogTileBlocked -> 0
    FogTileUnknown -> maxBound
    _ -> fromIntegral (fogTileAge tile)
