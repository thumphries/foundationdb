module Test.IO.FoundationDb.Process where


import           System.FilePath ((</>))
import qualified System.Process as P
import qualified System.IO.Temp as T


withTestDb :: IO a -> IO a
withTestDb f =
  -- fdbserver can't be started in tmpfs, uses O_DIRECT mode
  -- so we use the source tree as tmpdir here (and clean up)
  --
  -- TODO: maybe make this a Property and dump logs as counterexample
  T.withTempDirectory "test" "fdb" $ \dir -> do
    let cf = dir </> "fdb.cluster"
    writeFile cf clusterFile
    P.withCreateProcess
      ((P.shell "fdbserver -p auto:9090") {
          P.cwd = Just dir
        }) $ \_ _ _ _ -> f

clusterFile :: String
clusterFile =
  "test:DeDxzIdA@127.0.0.1:9090"
