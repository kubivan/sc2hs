module WaypointScore
  ( ScoutPath (..)
  , Waypoint
  , calcAlongPathScore
  , pathWaypoints
  ) where

import Data.Set qualified as Set
import Debug.Trace (traceM)
import Fmt ((+||), (||+), fmt)
import SC2.Grid
import SC2.Spatial (distManhattan, tilePos)
import SC2.TilePos (TilePos)
import StepMonad
  ( HasGrid
  , StepMonad
  , agentGrid
  )
import Units (Unit)

type Waypoint = (TilePos, Int)
type PathWaypoints = [Waypoint]

data ScoutPath = ScoutPath
  { scoutRoute :: [TilePos]
  , scoutWaypoints :: PathWaypoints
  }
  deriving (Eq, Show)

pathLen :: Grid -> TilePos -> TilePos -> Int
pathLen grid start dest = length path
 where
  GridBfsRes _ _ path =
    gridBfs
      grid
      start
      (getAllNotSharpNeighbors grid)
      (const (== dest))
      (const (const False))

pickWaypoint :: (HasGrid d) => TilePos -> [Waypoint] -> StepMonad d Waypoint
pickWaypoint u wps = do
  grid <- agentGrid
  let pathLen' wp = pathLen grid (tilePos u) (fst wp)
      waypointScore :: Waypoint -> Waypoint -> Float
      -- waypointScore p1 p2 = fromIntegral (pathLen' p1) / fromIntegral (pathLen' p1 + pathLen' p2)
      waypointScore p1 p2 = fromIntegral $ snd p1

  let [a, b] = closest2Waypoints grid (tilePos u) wps
  -- traceM $ "ab score: " ++ show a ++ " " ++ show (waypointScore a b)
  -- traceM $ "ba score: " ++ show b ++ " " ++ show (waypointScore b a)

  if waypointScore a b > waypointScore b a then pure a else pure b

-- looking for 2 points around tpos we're not standing at
closest2Waypoints :: Grid -> TilePos -> [Waypoint] -> [Waypoint]
closest2Waypoints grid tpos wps = firstTwoMatches visited
 where
  GridBfsRes _ visited _ = gridBfs grid tpos (getAllNotSharpNeighbors grid) acceptance (const (const False))
  firstTwoMatches :: Set.Set TilePos -> [Waypoint]
  -- looking for 2 ``unvisited`` - waypoints we're not standing at
  firstTwoMatches vs = take 2 (filter (\(p, _) -> Set.member p vs && distManhattan tpos p > 0) wps)
  acceptance vs _ = 2 == (length . firstTwoMatches $ vs)

pathWaypoints :: (HasGrid d) => TilePos -> TilePos -> StepMonad d ScoutPath
pathWaypoints start end = do
  grid <- agentGrid
  let distBetweenWaypoints = 2
      GridBfsRes isFound _ path = gridBfs grid start (getAllNotSharpNeighbors grid) (const (== end)) (const (const False))

  pure $
    case isFound of
      Nothing -> ScoutPath [] []
      Just _ ->
        ScoutPath
          path
          [ (wp, i)
          | (wp, i) <- zip (drop distBetweenWaypoints path) [distBetweenWaypoints ..]
          , i `mod` distBetweenWaypoints == 0 || wp == end
          ]

normalizeDelta :: Int -> Float
normalizeDelta delta =
  (fromIntegral delta + 1) / 2

calcAlongPathScore :: (HasGrid d) => [Waypoint] -> Unit -> TilePos -> StepMonad d Float
calcAlongPathScore wps u candidate = do
  grid <- agentGrid
  waypoint <- pickWaypoint (tilePos u) wps
  let currentDist = pathLen grid (tilePos u) (fst waypoint)
  traceM . fmt $ "picked WP=" +|| waypoint ||+ " currentDist=" +|| currentDist ||+ ""

  let d = pathLen grid candidate (fst waypoint)
  -- traceM $
  --   "candidate="
  --     ++ show candidate
  --     ++ " dist="
  --     ++ show d
  --     ++ " progress="
  --     ++ show (currentDist - d)
  --     ++ " openPathScore: "
  --     ++ show candidateOpenPathScore
  --     ++ " ageScore: "
  --     ++ show ageScore
  pure $ towardsWaypointScore grid (tilePos u) candidate waypoint

towardsWaypointScore :: Grid -> TilePos -> TilePos -> Waypoint -> Float
towardsWaypointScore grid current candidate (waypoint, _) =
  let currentDist = pathLen grid current waypoint
      candidateDist = pathLen grid candidate waypoint
   in -- normalize delta to [0..1]
      -- away -1 -> 0.0
      -- no change   0 -> 0.5
      -- toward     +1 -> 1.0
      normalizeDelta (currentDist - candidateDist)
