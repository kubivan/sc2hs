
import Grid
import Footprint
import qualified Data.Vector as V
import Footprint (getFootprint)
import UnitTypeId 
import UnitTypeId (UnitTypeId(ProtossNexus))

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
    let buildingFootprint = getFootprint ProtossNexus
    print buildingFootprint

    let myMap = createGrid mapRows
    let maybePlacementPoint = findPlacementPoint myMap buildingFootprint  (3,3)
    case maybePlacementPoint of
        Just placementPoint -> do
            putStrLn "\nPlacement Point:"
            putStrLn $ "Found placement point: " ++ show placementPoint

            let placedMap = return (addMark myMap buildingFootprint placementPoint) >>= \x -> updatePixel x placementPoint 'c'
            putStrLn "\nPlaced Map:"
            V.mapM_ putStrLn (V.toList <$> placedMap)
        Nothing -> putStrLn "No suitable placement point found"