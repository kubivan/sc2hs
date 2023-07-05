import Data.ProtoLens.Setup
import System.FilePath

protoDir = "protos"

main = do
  putStrLn "====================GENERATING========================"
  defaultMainGeneratingProtos protoDir
