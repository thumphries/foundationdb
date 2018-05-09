
import           Data.Traversable (sequenceA)

import           System.Exit (ExitCode (..), exitWith)

import qualified Test.IO.FoundationDb.C as C


main :: IO ()
main = do
  testMain [
      C.tests
    ]

testMain :: [IO Bool] -> IO ()
testMain tests = do
  bs <- sequenceA tests
  case and bs of
    True ->
      return ()
    False ->
      exitWith (ExitFailure 1)
