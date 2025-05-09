{-# OPTIONS -Wall #-}
-- {-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Grid.Algo (
    gridBfs,
    GridBfsRes (..),
    smartTransition,
    getAllNeighbors,
    getAllNotSharpNeighbors,
    findChokePoint,
    gridPlaceRay,
    gridSplitByRay,
    gridRaycastTile,
    findAllChokePoints,
    checkVolumes,
    gridSegment,
    buildRegionGraph,
    buildRegionLookup,
    RegionId,
    Region,
    RegionGraph,
)
where

import Grid.Core
import Utils (TilePos, dbg, distSquared)

import Control.Monad (guard)
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.List (find, sort)
import Data.Maybe (catMaybes, isJust)
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Debug.Trace (trace, traceM)
import Data.Map qualified as Map
import Data.HashMap.Strict(HashMap)
import Data.HashMap.Strict qualified as HashMap

type TilePath = [TilePos]

data GridBfsRes = GridBfsRes
    { bfsRes :: Maybe TilePos
    , bfsVisited :: Set.Set TilePos
    , bfsPath :: [TilePos]
    }
    deriving (Show)

gridBfs ::
    Grid -> TilePos -> (TilePos -> [TilePos]) -> (TilePos -> Bool) -> (TilePos -> Bool) -> GridBfsRes
gridBfs grid start transitionFunc acceptanceCriteria terminationCriteria =
    -- trace ("gridBfs start " ++ show start) $
    bfs (Seq.singleton (start, [start])) (Set.singleton start)
  where
    bfs Seq.Empty visited = GridBfsRes Nothing visited []
    bfs ((top, path) Seq.:<| queue) visited
        | acceptanceCriteria top = GridBfsRes (Just top) visited (reverse path) -- `Utils.dbg` ("gridBfs ended. visited: " ++ show (length visited))
        | terminationCriteria top = GridBfsRes Nothing visited []
        | otherwise = bfs queue' visited'
      where
        neighbors = filter (`Set.notMember` visited) (transitionFunc top)
        visited' = foldr Set.insert visited neighbors
        queue' :: Seq.Seq (TilePos, TilePath)
        queue' = queue Seq.>< (Seq.fromList $ (\n -> (n, n : path)) <$> neighbors)

smartTransition :: Grid -> [(Char, Char)] -> TilePos -> [TilePos]
smartTransition grid transitions pos@(x, y) = filter passTransitions allAdjacent
  where
    pixelFrom = gridPixel grid pos
    allAdjacent =
        [ (x + dx, y + dy)
        | (dx, dy) <- [(-1, 0), (1, 0), (0, -1), (0, 1)]
        , let pixel = grid !? (x + dx, y + dy)
        , isJust pixel
        ]
    passTransitions p = isJust $ find (canTransit p) transitions
    canTransit p (f, t) = res -- `Utils.dbg` (show pos ++ " : " ++show p ++ " " ++ show res ++ " :transition from " ++ show pixelFrom ++ " to " ++ show (gridPixel grid p))
      where
        res = pixelFrom == f && gridPixel grid p == t

getAllNeighbors :: Grid -> TilePos -> [TilePos]
getAllNeighbors grid (x, y) =
    [ (x + dx, y + dy)
    | dx <- [-1, 0, 1]
    , dy <- [-1, 0, 1]
    , dx /= 0 || dy /= 0 -- Exclude points on the same vertical line
    , let pixel = grid !? (x + dx, y + dy)
    , isJust pixel -- pixel /= Just '#'
    ]

getAllNotSharpNeighbors :: Grid -> TilePos -> [TilePos]
getAllNotSharpNeighbors grid (x, y) =
    [ (x + dx, y + dy)
    | dx <- [-1, 0, 1]
    , dy <- [-1, 0, 1]
    , dx /= 0 || dy /= 0 -- Exclude points on the same vertical line
    , let pixel = grid !? (x + dx, y + dy)
    , pixel /= Just '#'
    ]

--
-- Grid raycasting & choke points
--
type Ray = [TilePos]
type ChokeM = MaybeT (State (Grid, Set.Set Ray)) Ray

neighborsRay :: Grid -> TilePos -> [TilePos]
neighborsRay grid (x, y) =
    filter isValid [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
  where
    isValid (nx, ny) = case grid !? (nx, ny) of
        Just '#' -> False -- Only move on empty spaces
        Just '*' -> False -- Only move on empty spaces
        Nothing -> False
        _ -> True

gridPlaceRay :: Grid -> Ray -> Grid
gridPlaceRay = foldl' (\accGrid pixel -> gridSetPixel accGrid pixel '*')

gridRaycastTile :: Grid -> TilePos -> TilePos -> Maybe Ray
gridRaycastTile grid origin (dx, dy) =
    find isObstacle $ takeWhile inBounds $ iterate step [origin]
  where
    step ray@((px, py) : _) = (px + dx, py + dy) : ray
    inBounds ray = isJust $ grid !? head ray
    isObstacle ray = case grid !? head ray of
        Just '*' -> True
        Just '#' -> True
        Nothing -> True
        _ -> False

findChokePoint :: Grid -> Int -> TilePos -> Maybe [TilePos]
findChokePoint grid threshold start =
    find (\r -> rayShortEnough r && rayDoesntCross2Rays r) (zipRays <$> rays)
  where
    zipRays :: (Ray, Ray) -> Ray -- drop first elem from backward ray as it duplicates forward
    zipRays (forward, backward) = sort $ forward ++ (drop 1 . reverse $ backward) -- `Utils.dbg` ("zipping rays " ++ show (forward, backward))
    raycast = gridRaycastTile grid start
    rayDoesntCross2Rays :: Ray -> Bool
    rayDoesntCross2Rays ray = not $ grid !? head ray == Just '*' || grid !? last ray == Just '*'
    rayShortEnough :: Ray -> Bool
    rayShortEnough ray =
        threshold * threshold
            >= round
                ( distSquared (head ray) (last ray)
                -- `Utils.dbg` ("findChokePoint checking threshold " ++ show threshold ++ " " ++ show (ray) ++ " " ++ show (sqrt $ distSquared (head ray) (last ray)))
                )

    rays :: [(Ray, Ray)]
    rays =
        [ (forwardRay, backwardRay)
        | angle <- [0, 45, 90, 135, 180]
        , let rad = fromIntegral angle * pi / 180
        , let dir_forward = (round (cos rad), round (sin rad))
        , let dir_backward = (-fst dir_forward, -snd dir_forward)
        , Just forwardRay <- [raycast dir_forward]
        , Just backwardRay <- [raycast dir_backward]
        ]

gridSplitByRay :: Grid -> Int -> Ray -> (Maybe (Set.Set TilePos), Maybe (Set.Set TilePos))
gridSplitByRay grid minVolume' ray = checkFirst2 pointsAroundRay -- `Utils.dbg` ("pointsAroundRay: " ++ show pointsAroundRay)
  where
    minVolume = minVolume' * 2
    pointsAroundRay :: [TilePos]
    pointsAroundRay =
        Set.toList . Set.fromList $
            concatMap (neighborsRay grid) ray

    checkFirst2 :: [TilePos] -> (Maybe (Set.Set TilePos), Maybe (Set.Set TilePos))
    checkFirst2 [] = (Nothing, Nothing)
    checkFirst2 (a : ns) =
        let (visitedA, minVolumeReachedA) = gridFloodPeek grid (Set.fromList ray) minVolume a -- `Utils.dbg` ("gridFloodPeek:checking: " ++ show a)
            regionA = if minVolumeReachedA then Nothing else Just visitedA
            rest = filter (`Set.notMember` visitedA) ns
         in case rest of
                (b : _) ->
                    let (visitedB, minVolumeReachedB) = gridFloodPeek grid visitedA minVolume b
                     in trace
                            ( "A size: "
                                ++ show (Set.size visitedA, minVolumeReachedA)
                                ++ ", B size: "
                                ++ show (Set.size visitedB, minVolumeReachedB)
                            )
                            $ (regionA, if minVolumeReachedB then Nothing else Just visitedB)
                [] -> (regionA, Just Set.empty) -- trace "[checkFirst2] No disjoint B found" (visitedA, Set.empty)

checkVolumes :: Grid -> [TilePos] -> Int -> Bool
checkVolumes grid ray minVolume =
    case gridSplitByRay grid minVolume ray of
        (Just a, Just b) -> volumeA >= minVolume && volumeB >= minVolume -- `Utils.dbg` ("(Just a, Just b) ray " ++ show ray ++ " splits grid into volumes " ++ show (volumeA, volumeB))
          where
            volumeA = Set.size a
            volumeB = Set.size b
        (Just a, Nothing) -> volumeA >= minVolume -- `Utils.dbg` ("(Just a, Nothing) ray " ++ show ray ++ " splits grid into volumes " ++ show volumeA)
          where
            volumeA = Set.size a
        (Nothing, Just b) -> volumeB >= minVolume -- `Utils.dbg` ("(Nothing, Just b) ray " ++ show ray ++ " splits grid into volumes " ++ show volumeB)
          where
            volumeB = Set.size b
        d -> True `Utils.dbg` (show d ++ "ray " ++ show ray ++ " Doesn't splits grid into volumes, but ok")

gridFloodPeek :: Grid -> Set.Set TilePos -> Int -> TilePos -> (Set.Set TilePos, Bool)
gridFloodPeek grid visited minVolume start
    | start `Set.member` visited = (Set.empty, False)
    | otherwise = (region, minVolumeReached)
  where
    (finalVisited, minVolumeReached) = bfs (Seq.singleton start) (Set.insert start visited)
    region = Set.difference finalVisited visited
    firstRegionSize = Set.size visited

    bfs :: Seq.Seq TilePos -> Set.Set TilePos -> (Set.Set TilePos, Bool)
    bfs Seq.Empty vis = (vis, False)
    bfs (curr Seq.:<| queue) vis
        | Set.size vis - firstRegionSize > minVolume = (vis, True)
        | otherwise =
            let newNeighbors = filter (`Set.notMember` vis) (neighborsRay grid curr)
                vis' = foldr Set.insert vis newNeighbors
                queue' = queue Seq.>< Seq.fromList newNeighbors
             in bfs queue' vis'

checkIfChoke :: TilePos -> ChokeM
checkIfChoke pos = do
    -- traceM $ "checkIfChoke: " ++ show pos
    (grid, visited) <- lift get

    guard (gridPixel grid pos `notElem` ['#', '*'])

    ray <- MaybeT $ return $ findChokePoint grid 15 pos -- Lift Maybe into ChokeM
    guard (ray /= [])

    -- TODO: check & enable
    -- guard (all (all (\c -> distSquared pos c > 4 * 4)) currentChokes)

    guard (ray `Set.notMember` visited)
    lift $ put (grid, Set.insert ray visited)
    -- traceM "Ray not yet checked"

    -- Update state
    let grid' = gridPlaceRay grid ray
    traceM "checking volumes"
    -- let volumesRes = checkVolumes grid' ray 100
    let volumesRes = checkVolumes grid' ray 250
    guard (trace ("volume check " ++ show volumesRes) volumesRes)
    traceM "volumes check passed!!!!"

    lift $ put (grid', Set.insert ray visited)

    traceM "volumes check passed2!!!"
    return ray

findAllChokePoints :: Grid -> ([Ray], Grid)
findAllChokePoints grid =
    let openCells =
            [ (x, y)
            | y <- [0 .. gridH grid - 1]
            , x <- [0 .. gridW grid - 1]
            , gridPixel grid (x, y) /= '#'
            ]

        runEach pos = runMaybeT (checkIfChoke pos)

        (maybeRays, (grid', _)) = runState (mapM runEach openCells) (grid, Set.empty)
     in (catMaybes maybeRays, grid')

gridSegment :: Grid -> [(Int, Set.Set TilePos)]
gridSegment grid =
    go 0 openCells
  where
    openCells =
        Set.fromList
            [ (x, y)
            | y <- [0 .. gridH grid - 1]
            , x <- [0 .. gridW grid - 1]
            , gridPixel grid (x, y) == ' '
            ]

    go id rest
        | trace (show id ++ " openCells: " ++ show (Set.size rest)) False = undefined
        | Set.null rest = []
        | otherwise =
            let start = Set.findMin rest
                region = fillRegion' start
                rest' = rest `Set.difference` region
             in trace ("found region " ++ show (Set.size region) ++ " rest to check: " ++ show (Set.size rest')) $ case Set.minView rest' of
                    Nothing -> [(id, region)]
                    _ -> (id, region) : go (id + 1) rest'

    fillRegion' pos =
        bfsVisited $ gridBfs grid pos (smartTransition grid [(' ', ' ')]) (const False) (const False)

type RegionId = Int
type Region = Set.Set TilePos
type RegionGraph = HashMap RegionId (Set.Set RegionId)
type RegionLookup = HashMap TilePos RegionId

-- Get 4-connected neighbors (no diagonals)
adjacent4 :: TilePos -> [TilePos]
adjacent4 (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

buildRegionLookup :: [(RegionId, Region)] -> RegionLookup
buildRegionLookup regions =
    HashMap.fromList
        [(pos, rid) | (rid, region) <- regions, pos <- Set.toList region]

buildRegionGraph :: [(RegionId, Region)] -> RegionGraph
buildRegionGraph regions =
    HashMap.fromListWith
        Set.union
        [ (rid, Set.singleton rid')
        | (rid, region) <- regions
        , pos <- Set.toList region
        , neighbor <- adjacent4 pos
        , Just rid' <- [HashMap.lookup neighbor regionLookup]
        , rid /= rid' -- skip self
        ]
  where
    regionLookup = buildRegionLookup regions
