
module SC2.Config (
    hostName,
    portHost,
    portClient,
    serverPortSet,
    clientPortSet)
where
import Data.Int (Int32)
import  qualified GHC.Int

hostName :: String
hostName = "127.0.0.1"

-- port :: Int
-- port = 8167
portHost :: GHC.Int.Int32
portHost = 8167
portClient :: GHC.Int.Int32
portClient = portHost + 1 -- 8168

serverPortSet :: (Int32, Int32)
serverPortSet = (portHost + 2, portHost + 3)
clientPortSet :: (Int32, Int32)
clientPortSet = (portHost + 4, portHost + 5)