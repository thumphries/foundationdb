
import           Control.Concurrent
import qualified Control.Concurrent.Async as A

import           Data.Foldable
import           Data.Traversable (sequenceA)

import qualified FoundationDb.C as C

import           System.Exit (ExitCode (..), exitWith)
import qualified System.IO as IO

import qualified Test.IO.FoundationDb.C as C


main :: IO ()
main = do
  IO.hSetBuffering IO.stdout IO.LineBuffering
  IO.hSetBuffering IO.stderr IO.LineBuffering
  setup
  testMain [
      C.tests
    ]

-- Move all library setup up front
setup :: IO ()
setup = do
  C.selectApiVersion
  C.setupNetwork
  for_ [1..5::Int] $ \_ -> do
    putStrLn "Starting network thread..."
    A.withAsyncBound C.runNetwork $ \networkThread -> do
      cf <- C.createCluster "fdb.cluster"
      C.futureBlockUntilReady cf
      cluster <- C.futureGetCluster cf
      C.destroyCluster cluster
      C.stopNetwork
      A.wait networkThread
      threadDelay 1000000

testMain :: [IO Bool] -> IO ()
testMain tests = do
  bs <- sequenceA tests
  case and bs of
    True ->
      return ()
    False ->
      exitWith (ExitFailure 1)
