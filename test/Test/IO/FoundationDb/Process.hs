module Test.IO.FoundationDb.Process where


import           Control.Concurrent
import qualified Control.Concurrent.Async as A
import           Control.Exception (finally)

import Debug.Trace

import qualified FoundationDb.C as C
import           FoundationDb.C.Types

import           System.FilePath ((</>))
import qualified System.Process as P
import qualified System.IO.Temp as T


withTestDb :: (Cluster -> Database -> IO r) -> IO r
withTestDb f =
  withTestServer $ \cfile -> do
    A.withAsyncBound C.runNetwork $ \networkThread ->
      go cfile `finally` (C.stopNetwork >> A.wait networkThread)
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
    P.withCreateProcess (shellInDir "fdbserver -p auto:9090" dir) $ \_ _ _ ph -> do
      threadDelay 1000000
      putStrLn "starting fdbcli"
      out <- P.readCreateProcess (shellInDir "fdbcli --exec \"configure new single memory ; status\"" dir) []
      putStrLn "done"
      putStrLn out
      traceM out
      f cf `finally` (P.terminateProcess ph >> P.waitForProcess ph)


shellInDir :: String -> FilePath -> P.CreateProcess
shellInDir cmd dir =
  (P.shell cmd) {
      P.cwd = Just dir
    , P.std_out = P.Inherit
    , P.std_err = P.Inherit
    }

clusterFile :: String
clusterFile =
  "test:DeDxzIdA@127.0.0.1:9090"
