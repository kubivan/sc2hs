{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Agent (Agent (..), StepPlan (..))
import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, bracket, catch)
import Data.List (isInfixOf)
import Data.ProtoLens.Labels ()
import Data.Text qualified as Text
import Lens.Micro ((^.))
import Network.WebSockets qualified as WS
import Options.Applicative
import Proto.S2clientprotocol.Common qualified as C
import Proto.S2clientprotocol.Sc2api qualified as A
import SC2.Grid (gridFromImage, gridMerge, gridToFile, pixelIsRamp)
import SC2.Launcher.Participant (Participant (..))
import SC2.Proto.Data (Map (LocalMap), Race)
import SC2.Proto.Requests qualified as Proto
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, takeFileName)
import System.Process (readProcessWithExitCode)

data Cli = Cli
    { cliMapPath :: FilePath
    , cliOutputPath :: FilePath
    , cliHost :: String
    , cliPort :: Int
    , cliDockerImage :: String
    , cliContainerName :: String
    }

data DockerStart
    = DockerStarted
    | DockerReuseExisting

newtype DumpAgent = DumpAgent Race

instance Agent DumpAgent where
    makeAgent agent _ _ _ _ = pure agent
    agentRace (DumpAgent race) = race
    agentStep agent _ _ = pure (agent, StepPlan [] [] [])

main :: IO ()
main = do
    cli <- execParser parserInfo
    let mapFileName = takeFileName (cliMapPath cli)
        mapDir = takeDirectory (cliMapPath cli)
        containerMapPath = "/StarCraftII/maps/" <> mapFileName
        playerRace = C.Protoss
        aiRace = C.Random
        aiDifficulty = A.VeryEasy

    createDirectoryIfMissing True (takeDirectory (cliOutputPath cli))

    bracket
        (startContainer cli mapDir)
        (\startMode -> case startMode of
            DockerStarted -> stopContainer (cliContainerName cli)
            DockerReuseExisting -> pure ()
        )
        ( \_ -> do
            waitForSc2 (cliHost cli) (cliPort cli)
            fetchAndDumpGrid cli containerMapPath playerRace aiRace aiDifficulty
        )

parserInfo :: ParserInfo Cli
parserInfo =
    info
        (cliParser <**> helper)
        ( fullDesc
            <> progDesc "Start SC2 in Docker, create a game from map path, and dump merged traversal grid to txt"
        )

cliParser :: Parser Cli
cliParser =
    Cli
        <$> strOption (long "map-path" <> metavar "PATH" <> help "Host path to .SC2Map file")
        <*> strOption (long "output" <> metavar "PATH" <> help "Output txt path for merged grid")
        <*> strOption (long "host" <> value "127.0.0.1" <> showDefault <> help "SC2 websocket host")
        <*> option auto (long "port" <> value 5555 <> showDefault <> help "SC2 websocket port")
        <*> strOption
            ( long "docker-image"
                <> value "ghcr.io/kubivan/aiurgaze-sc2:latest"
                <> showDefault
                <> help "Docker image with headless SC2"
            )
        <*> strOption
            ( long "container-name"
                <> value "sc2mapdump-sc2"
                <> showDefault
                <> help "Docker container name"
            )

startContainer :: Cli -> FilePath -> IO DockerStart
startContainer cli mapsDir = do
    _ <- readProcessWithExitCode "docker" ["rm", "-f", cliContainerName cli] ""
    runDockerChecked ["pull", cliDockerImage cli]
    let mapsMount = mapsDir <> ":/StarCraftII/maps"
        portMap = show (cliPort cli) <> ":" <> show (cliPort cli)
    (exitCode, out, err) <-
        readProcessWithExitCode
            "docker"
            [ "run"
            , "-d"
            , "--rm"
            , "--name"
            , cliContainerName cli
            , "-p"
            , portMap
            , "-v"
            , mapsMount
            , cliDockerImage cli
            ]
            ""

    case exitCode of
        ExitSuccess -> pure DockerStarted
        ExitFailure code
            | "port is already allocated" `elem` words err
                || "port is already allocated" `isInfixOf` err -> do
                    putStrLn $ "Port " <> show (cliPort cli) <> " is already allocated; reusing existing SC2 endpoint at " <> cliHost cli <> ":" <> show (cliPort cli)
                    pure DockerReuseExisting
            | otherwise ->
                error $ "docker run -d --rm --name " <> cliContainerName cli <> " -p " <> portMap <> " -v " <> mapsMount <> " " <> cliDockerImage cli <> " failed (" <> show code <> ")\nstdout:\n" <> out <> "\nstderr:\n" <> err

stopContainer :: String -> IO ()
stopContainer containerName = do
    _ <- readProcessWithExitCode "docker" ["rm", "-f", containerName] ""
    pure ()

runDockerChecked :: [String] -> IO ()
runDockerChecked args = do
    (exitCode, out, err) <- readProcessWithExitCode "docker" args ""
    case exitCode of
        ExitSuccess -> pure ()
        ExitFailure code ->
            error $ "docker " <> unwords args <> " failed (" <> show code <> ")\nstdout:\n" <> out <> "\nstderr:\n" <> err

waitForSc2 :: String -> Int -> IO ()
waitForSc2 host port = loop (120 :: Int)
  where
    loop 0 = error "Timed out waiting for SC2 websocket"
    loop attemptsLeft = do
        result <-
            catch
                ( WS.runClient host port "/sc2api" (\conn -> Proto.sendRequestSync conn Proto.requestPing >> pure True)
                )
                (\(_ :: SomeException) -> pure False)
        if result
            then pure ()
            else threadDelay 1000000 >> loop (attemptsLeft - 1)

fetchAndDumpGrid :: Cli -> FilePath -> Race -> Race -> A.Difficulty -> IO ()
fetchAndDumpGrid cli containerMapPath playerRace aiRace aiDifficulty =
    WS.runClient (cliHost cli) (cliPort cli) "/sc2api" $ \conn -> do
        let hostPlayer = Player (DumpAgent playerRace)
            aiPlayer = Computer aiRace aiDifficulty Nothing
            createReq = Proto.requestCreateGame (LocalMap (Text.pack containerMapPath) Nothing) [hostPlayer, aiPlayer]
            joinReq = Proto.requestJoinGameVsAi playerRace

        _ <- Proto.sendRequestSync conn createReq
        _ <- Proto.sendRequestSync conn joinReq
        responseGameInfo <- Proto.sendRequestSync conn Proto.requestGameInfo

        let gameInfo = responseGameInfo ^. #gameInfo
            placementGrid = gridFromImage (gameInfo ^. #startRaw . #placementGrid)
            pathingGrid = gridFromImage (gameInfo ^. #startRaw . #pathingGrid)
            merged = gridMerge pixelIsRamp placementGrid pathingGrid

        gridToFile (cliOutputPath cli) merged
