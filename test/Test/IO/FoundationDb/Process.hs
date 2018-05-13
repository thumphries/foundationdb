module Test.IO.FoundationDb.Process where


import qualified Control.Concurrent.Async as A
import           Control.Exception (finally)

import qualified FoundationDb.C as C
import           FoundationDb.C.Types

import           System.FilePath ((</>))
import qualified System.Process as P
import qualified System.IO.Temp as T


withTestDb :: (Cluster -> Database -> IO r) -> IO r
withTestDb f =
  withTestServer $ \cfile -> do
    C.selectApiVersion
    C.setupNetwork
    A.withAsyncBound C.runNetwork $ \_ ->
      go cfile `finally` C.stopNetwork
  where
    go cf = do
      cfuture <- C.createCluster cf
      C.futureBlockUntilReady cfuture
      cluster <- C.futureGetCluster cfuture
      dfuture <- C.createDatabase cluster
      C.futureBlockUntilReady dfuture
      database <- C.futureGetDatabase dfuture
      f cluster database
        `finally` cleanup cluster database
    cleanup c d = do
      C.destroyDatabase d
      C.destroyCluster c

withTestServer :: (FilePath -> IO a) -> IO a
withTestServer f =
  -- fdbserver can't be started in tmpfs, uses O_DIRECT mode
  -- so we use the source tree as tmpdir here (and clean up)
  --
  -- TODO: maybe make this a Property and dump logs as counterexample
  T.withTempDirectory "test" "fdb" $ \dir -> do
    let cf = dir </> "fdb.cluster"
    writeFile cf clusterFile
    P.withCreateProcess (shellInDir "fdbserver -p auto:9090" dir) $ \_ _ _ _ -> do
      _ <- P.readCreateProcess (shellInDir "fdbcli --exec \"configure new single memory ; status\"" dir) []
      f cf

shellInDir :: String -> FilePath -> P.CreateProcess
shellInDir cmd dir =
  (P.shell cmd) {
      P.cwd = Just dir
    }

clusterFile :: String
clusterFile =
  "test:DeDxzIdA@127.0.0.1:9090"
