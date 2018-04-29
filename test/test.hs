
import           Data.Traversable (sequenceA)
import           System.Exit (ExitCode (..), exitWith)


main :: IO ()
main = do
  testMain [
    ]

testMain :: [IO Bool] -> IO ()
testMain tests = do
  bs <- sequenceA tests
  case and bs of
    True ->
      return ()
    False ->
      exitWith (ExitFailure 1)
