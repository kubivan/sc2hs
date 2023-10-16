
import Grid
import qualified Data.Vector as V

main :: IO ()
main = do
    let mapRows =
            [ "###########"
            , "#         #"
            , "#         #"
            , "#         #"
            , "#         #"
            , "#         #"
            , "###########"
            ]
    let buildingStr = unlines
            [ " bb"
            , "bcb"
            , "bbb"
            ]
    let buildingFootprint = createFootprint buildingStr
    print buildingFootprint

    let myMap = createGrid mapRows
    let maybePlacementPoint = findPlacementPoint myMap buildingFootprint  (0,0)
    case maybePlacementPoint of
        Just placementPoint -> do
            putStrLn "\nPlacement Point:"
            putStrLn $ "Found placement point: " ++ show placementPoint

            let placedMap = placeBuilding myMap buildingFootprint placementPoint
            putStrLn "\nPlaced Map:"
            V.mapM_ putStrLn (V.toList <$> placedMap)
        Nothing -> putStrLn "No suitable placement point found"