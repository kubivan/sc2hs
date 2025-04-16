-- {-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Grid (
    gridFromImage,
    gridToStr,
    gridFromLines,
    Grid,
    findPlacementPoint,
    findPlacementPointInRadius,
    canPlaceBuilding,
    gridBfs,
    gridPlace,
    addMark,
    printGrid,
    gridToFile,
    gridSetPixel,
    gridPixel,
    gridPixelSafe,
    gridMerge,
    pixelIsRamp,
    gridW,
    gridH,
    gridRaycastTile,
    findChokePoint,
    getAllNeigbors,
    getAllNotSharpNeigbors,

    gridPlaceRay,
    gridSplitByRay,
    gridRaycastTile,
    findAllChokePoints,
    checkVolumes
)
where

import Data.Bits
import Data.ByteString qualified as BS
import Data.List (find, sort)
import Data.Vector qualified as V
import Data.Word (Word8)

import Data.Set qualified as Set
import Lens.Micro ((^.))

import Proto.S2clientprotocol.Common qualified as P
import Proto.S2clientprotocol.Common_Fields qualified as P

import Data.Foldable (toList)
import Data.Maybe (isJust)
import Footprint
import Utils (TilePos, dbg, distSquared, fromTuple)

import Data.Sequence qualified as Seq -- (Seq (..), empty, (|>))
import Debug.Trace (trace)
import UnitTypeId (UnitTypeId)

-- raycasting
import Control.Monad.Trans.Maybe
import Control.Monad.State
import Control.Monad (guard, mplus)
import Data.Maybe (catMaybes, fromMaybe)
import Debug.Trace (traceShow, traceShowId, traceM)

type Grid = V.Vector (V.Vector Char)

gridH :: Grid -> Int
gridH = V.length

gridW :: Grid -> Int
gridW = V.length . V.head

gridPixel :: Grid -> TilePos -> Char
gridPixel g (x, y) = (g V.! y) V.! x

gridPixelSafe :: Grid -> TilePos -> Maybe Char
gridPixelSafe g (x, y) = g V.!? y >>= (V.!? x)

gridFromLines :: [String] -> Grid
gridFromLines rows = V.fromList [V.fromList row | row <- rows]

gridToStr :: Grid -> String
gridToStr g = unlines $ V.toList $ V.toList <$> V.reverse g

gridFromImage :: P.ImageData -> Grid
gridFromImage image = decodeImageData width height bpp bs
  where
    width = fromIntegral $ image ^. (P.size . P.x)
    height = fromIntegral $ image ^. (P.size . P.y)
    bpp = fromIntegral $ image ^. P.bitsPerPixel
    bs = image ^. P.data' :: BS.ByteString

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

pixelIsRamp :: Char -> Char -> Char
pixelIsRamp placement pathing
    | pathing == ' ' && placement == '#' = '/'
    | otherwise = placement

gridMerge :: (Char -> Char -> Char) -> Grid -> Grid -> Grid
gridMerge pixelFunc placementGrid pathingGrid =
    V.fromList
        [ V.fromList
            [pixelFunc (gridPixel placementGrid (x, y)) (gridPixel pathingGrid (x, y)) | x <- [0 .. (gridW placementGrid - 1)]]
        | y <- [0 .. (gridH placementGrid - 1)]
        ]
gridToFile :: FilePath -> Grid -> IO ()
gridToFile filePath grid =
    writeFile filePath (gridToStr grid)

printGrid :: Grid -> IO ()
printGrid = putStrLn . gridToStr

-- Decode a single byte into a list of bits.
decodeByte :: Word8 -> [Bool]
decodeByte byte = [testBit byte i | i <- [0 .. 7]]

canPlaceBuilding :: Grid -> Grid -> TilePos -> Footprint -> Bool
canPlaceBuilding grid heightMap (cx, cy) (Footprint pixels) =
    all pixelOk pixels && sameHeight pixels
  where
    pixelOk (x, y, _) = case gridPixelSafe grid (cx + x, cy + y) of
        Nothing -> False
        Just p -> p == ' ' -- TODO: now . has multiple meanings, rework
    sameHeight pixels = all (== head pixelHeights) (tail pixelHeights)
      where
        pixelHeights = [gridPixel heightMap pos | (x, y, _) <- pixels, let pos = (cx + x, cy + y)]

findPlacementPointInRadius :: Grid -> Grid -> Footprint -> TilePos -> Float -> Maybe TilePos
findPlacementPointInRadius grid heightMap footprint start radius =
    gridBfs grid start getAllNeigbors acceptWhen terminateWhen
  where
    acceptWhen p = canPlaceBuilding grid heightMap p footprint
    terminateWhen p = distSquared (fromTuple start) (fromTuple p) > (radius * radius)

findPlacementPoint :: Grid -> Grid -> Footprint -> TilePos -> (TilePos -> Bool) -> Maybe TilePos
findPlacementPoint grid heightMap footprint start acceptanceCriteria = gridBfs grid start getAllNeigbors acceptance (const False)
  where
    acceptance p = canPlaceBuilding grid heightMap p footprint && acceptanceCriteria p

getAllNeigbors :: Grid -> TilePos -> Seq.Seq TilePos
getAllNeigbors grid (x, y) =
    Seq.fromList
        [ (x + dx, y + dy)
        | dx <- [-1, 0, 1]
        , dy <- [-1, 0, 1]
        , dx /= 0 || dy /= 0 -- Exclude points on the same vertical line
        , let pixel = gridPixelSafe grid (x + dx, y + dy)
        , isJust pixel -- pixel /= Just '#'
        ]

getAllNotSharpNeigbors :: Grid -> TilePos -> Seq.Seq TilePos
getAllNotSharpNeigbors grid (x, y) =
    Seq.fromList
        [ (x + dx, y + dy)
        | dx <- [-1, 0, 1]
        , dy <- [-1, 0, 1]
        , dx /= 0 || dy /= 0 -- Exclude points on the same vertical line
        , let pixel = gridPixelSafe grid (x + dx, y + dy)
        , pixel /= Just '#'
        ]

gridBfs ::
    Grid -> TilePos -> (Grid -> TilePos -> Seq.Seq TilePos) -> (TilePos -> Bool) -> (TilePos -> Bool) -> Maybe TilePos
gridBfs grid start transitionFunc acceptanceCriteria terminationCriteria =
    bfs (Seq.singleton start) (Set.singleton start)
  where
    bfs Seq.Empty _ = Nothing
    bfs (top Seq.:<| rest) visited
        | acceptanceCriteria top = Just top `Utils.dbg` ("gridBfs ended. visited: " ++ show (length visited))
        | terminationCriteria top = Nothing
        | otherwise = bfs (rest Seq.>< newPoints) newVisited
      where
        newPoints = Seq.filter (`Set.notMember` visited) (transitionFunc grid top)
        newVisited = Set.union visited (Set.fromList . toList $ newPoints)

-- Place a building footprint on the grid if possible at the given placement point
addMark :: Grid -> Footprint -> TilePos -> Grid
addMark grid (Footprint pixels) (cx, cy) =
    foldl (\accGrid (x, y, mark) -> gridSetPixel accGrid (cx + x, cy + y) mark) grid pixels

gridPlace :: Grid -> UnitTypeId -> TilePos -> Grid
gridPlace g u (cx, cy) = foldl (\accGrid (x, y, mark) -> gridSetPixel accGrid (cx + x, cy + y) mark) g ptrn
  where
    ptrn = pixels (getFootprint u)

-- Update a cell in the Grid
gridSetPixel :: Grid -> TilePos -> Char -> Grid
gridSetPixel grid p@(i, j) value = if currentPixel == '#' then grid else grid V.// [(j, (grid V.! j) V.// [(i, value)])]
  where
    currentPixel = gridPixel grid p

--
-- Grid raycasting & choke points
--
type Ray = [TilePos]
type ChokeM = MaybeT (State (Grid, Set.Set Ray)) Ray

neighborsRay :: Grid -> TilePos -> [TilePos]
neighborsRay grid (x, y) =
    filter isValid [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
  where
    isValid (nx, ny) = case gridPixelSafe grid (nx, ny) of
        Just '#' -> False -- Only move on empty spaces
        Just '*' -> False -- Only move on empty spaces
        Nothing -> False
        _ -> True

gridPlaceRay :: Grid -> Ray -> Grid
gridPlaceRay = foldl (\accGrid pixel -> gridSetPixel accGrid pixel '*')

gridRaycastTile :: Grid -> TilePos -> TilePos -> Maybe Ray
gridRaycastTile grid origin (dx, dy) =
    find isObstacle $ takeWhile inBounds $ iterate step [origin]
  where
    step ray@((px, py) : _) = (px + dx, py + dy) : ray
    inBounds ray = isJust $ gridPixelSafe grid (head ray)
    isObstacle ray = case gridPixelSafe grid (head ray) of
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
    rayDoesntCross2Rays ray = not $ gridPixelSafe grid (head ray) == Just '*' || gridPixelSafe grid (last ray) == Just '*'
    rayShortEnough :: Ray -> Bool
    rayShortEnough ray =
        threshold * threshold
            >= round
                ( distSquared (head ray) (last ray)
                 --`Utils.dbg` ("findChokePoint checking threshold " ++ show threshold ++ " " ++ show (ray) ++ " " ++ show (sqrt $ distSquared (head ray) (last ray)))
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
gridSplitByRay grid minVolume' ray = checkFirst2 pointsAroundRay --`Utils.dbg` ("pointsAroundRay: " ++ show pointsAroundRay)
  where
    minVolume = minVolume' * 2
    pointsAroundRay :: [TilePos]
    pointsAroundRay =
            Set.toList . Set.fromList $
                concatMap (neighborsRay grid) ray

    checkFirst2 :: [TilePos] -> (Maybe (Set.Set TilePos), Maybe (Set.Set TilePos))
    checkFirst2 [] = (Nothing, Nothing)
    checkFirst2 (a : ns) =
        let (visitedA, minVolumeReachedA) = gridFloodPeek grid (Set.fromList ray) minVolume a `Utils.dbg` ("gridFloodPeek:checking: " ++ show a)
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
        (Just a, Just b) -> volumeA >= minVolume && volumeB >= minVolume `Utils.dbg` ("(Just a, Just b) ray " ++ show ray ++ " splits grid into volumes " ++ show (volumeA, volumeB))
            where
                volumeA = Set.size a
                volumeB = Set.size b
        (Just a, Nothing) -> volumeA >= minVolume `Utils.dbg` ("(Just a, Nothing) ray " ++ show ray ++ " splits grid into volumes " ++ show volumeA)
            where
                volumeA = Set.size a
        (Nothing, Just b) -> volumeB >= minVolume `Utils.dbg` ("(Nothing, Just b) ray " ++ show ray ++ " splits grid into volumes " ++ show volumeB)
            where
                volumeB = Set.size b
        d -> True `Utils.dbg` (show d ++ "ray " ++ show ray ++ " Doesn't splits grid into volumes, but ok")


gridFloodPeek :: Grid -> Set.Set TilePos -> Int -> TilePos -> (Set.Set TilePos, Bool)
gridFloodPeek grid visited minVolume start
  | start `Set.member` visited = (Set.empty, False)
  | otherwise = (region, minVolumeReached) where
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
    --traceM $ "checkIfChoke: " ++ show pos
    (grid, visited) <- lift get

    guard (gridPixel grid pos `notElem` ['#', '*'])

    ray <- MaybeT $ return $ findChokePoint grid 15 pos  -- Lift Maybe into ChokeM
    guard (ray /= [])

    -- guard (all (all (\c -> distSquared pos c > 4 * 4)) currentChokes)

    guard (ray `Set.notMember` visited)
    lift $ put (grid, Set.insert ray visited)
    --traceM "Ray not yet checked"

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
