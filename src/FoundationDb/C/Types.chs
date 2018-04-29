{-# LANGUAGE EmptyDataDecls #-}
module FoundationDb.C.Types (
    Future (..)
  , Future'
  , Transaction (..)
  , Transaction'
  , Callback (..)
  , Param (..)
  , Param'
  , Cluster (..)
  , Cluster'
  , Database (..)
  , Database'
  , NetworkOption (..)
  , StreamingMode (..)
  , CError (..)
  , csuccess
  , cbool
  , boolc
  ) where


import           Foreign
import           Foreign.C


#define FDB_API_VERSION 510
#include <foundationdb/fdb_c.h>
#include <foundationdb/fdb_c_options.g.h>


-- TODO list
-- Opaque types: Database, Cluster, FDBFuture
-- NetworkOption

-- Library setup / teardown:
-- fdb_select_api_version
-- fdb_setup_network
-- fdb_network_set_option
-- fdb_run_network
-- fdb_stop_network

-- FDBCallback

-- FDBFuture

-- FDBTransaction family


newtype CError = CError {
    unCError :: CInt
  } deriving (Eq, Ord, Show)

csuccess :: CError -> Bool
csuccess = (== 0) . unCError

cbool :: CInt -> Bool
cbool 0 = False
cbool _ = True

boolc :: Bool -> CInt
boolc False = 0
boolc True = 1

newtype Future = Future {
    unFuture :: Ptr Future'
  } deriving (Eq, Ord, Show)

data Future'

newtype Transaction = Transaction {
    unTransaction :: Ptr Transaction'
  } deriving (Eq, Ord, Show)

data Transaction'


newtype Callback = Callback {
    unCallback :: FunPtr (Ptr Future' -> Ptr Param' -> IO ())
  }

newtype Param = Param {
    unParam :: Ptr Param'
  } deriving (Eq, Ord, Show)

data Param'

newtype Database = Database {
    unDatabase :: Ptr Database'
  } deriving (Eq, Ord, Show)

data Database'

newtype Cluster = Cluster {
    unCluster :: Ptr Cluster'
  } deriving (Eq, Ord, Show)

data Cluster'

{#enum FDBNetworkOption as NetworkOption
   { FDB_NET_OPTION_LOCAL_ADDRESS as LOCAL_ADDRESS
   , FDB_NET_OPTION_CLUSTER_FILE as CLUSTER_FILE
   , FDB_NET_OPTION_TRACE_ENABLE as TRACE_ENABLE
   , FDB_NET_OPTION_TRACE_ROLL_SIZE as TRACE_ROLL_SIZE
   , FDB_NET_OPTION_TRACE_MAX_LOGS_SIZE as TRACE_MAX_LOGS_SIZE
   , FDB_NET_OPTION_TRACE_LOG_GROUP as TRACE_LOG_GROUP
   , FDB_NET_OPTION_KNOB as KNOB
   , FDB_NET_OPTION_TLS_PLUGIN as TLS_PLUGIN
   , FDB_NET_OPTION_TLS_CERT_BYTES as TLS_CERT_BYTES
   , FDB_NET_OPTION_TLS_CERT_PATH as TLS_CERT_PATH
   , FDB_NET_OPTION_TLS_KEY_BYTES as TLS_KEY_BYTES
   , FDB_NET_OPTION_TLS_KEY_PATH as TLS_KEY_PATH
   , FDB_NET_OPTION_TLS_VERIFY_PEERS as TLS_VERIFY_PEERS
   , FDB_NET_OPTION_BUGGIFY_ENABLE as BUGGIFY_ENABLE
   , FDB_NET_OPTION_BUGGIFY_DISABLE as BUGGIFY_DISABLE
   , FDB_NET_OPTION_BUGGIFY_SECTION_ACTIVATED_PROBABILITY as BUGGIFY_SECTION_ACTIVATED_PROBABILITY
   , FDB_NET_OPTION_BUGGIFY_SECTION_FIRED_PROBABILITY as BUGGIFY_SECTION_FIRED_PROBABILITY
   , FDB_NET_OPTION_DISABLE_MULTI_VERSION_CLIENT_API as DISABLE_MULTI_VERSION_CLIENT_API
   , FDB_NET_OPTION_CALLBACKS_ON_EXTERNAL_THREADS as CALLBACKS_ON_EXTERNAL_THREADS
   , FDB_NET_OPTION_EXTERNAL_CLIENT_LIBRARY as EXTERNAL_CLIENT_LIBRARY
   , FDB_NET_OPTION_EXTERNAL_CLIENT_DIRECTORY as EXTERNAL_CLIENT_DIRECTORY
   , FDB_NET_OPTION_DISABLE_LOCAL_CLIENT as DISABLE_LOCAL_CLIENT
   , FDB_NET_OPTION_DISABLE_CLIENT_STATISTICS_LOGGING as DISABLE_CLIENT_STATISTICS_LOGGING
   , FDB_NET_OPTION_ENABLE_SLOW_TASK_PROFILING as ENABLE_SLOW_TASK_PROFILING
   } deriving (Eq, Ord, Show)
#}

{#enum FDBStreamingMode as StreamingMode
  { FDB_STREAMING_MODE_ITERATOR as ITERATOR
  , FDB_STREAMING_MODE_SMALL as SMALL
  , FDB_STREAMING_MODE_MEDIUM as MEDIUM
  , FDB_STREAMING_MODE_LARGE as LARGE
  , FDB_STREAMING_MODE_SERIAL as SERIAL
  , FDB_STREAMING_MODE_WANT_ALL as WANT_ALL
  , FDB_STREAMING_MODE_EXACT as EXACT
  } deriving (Eq, Ord, Show)
#}
