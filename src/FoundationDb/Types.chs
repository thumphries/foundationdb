{-# LANGUAGE EmptyDataDecls #-}
module FoundationDb.Types (
    Database
  , Transaction
  , Cluster
  , Future
  , NetworkOption
  , StreamingMode (..)
  ) where

#define FDB_API_VERSION 510
#include <foundationdb/fdb_c.h>
#include <foundationdb/fdb_c_options.g.h>


-- TODO list
-- Opaque types: Database, Transaction, Cluster, FDBFuture
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


data Database

data Transaction

data Cluster

data Future

data NetworkOption

{#enum FDBStreamingMode as StreamingMode
  { FDB_STREAMING_MODE_ITERATOR as Iterator
  , FDB_STREAMING_MODE_SMALL as Small
  , FDB_STREAMING_MODE_MEDIUM as Medium
  , FDB_STREAMING_MODE_LARGE as Large
  , FDB_STREAMING_MODE_SERIAL as Serial
  , FDB_STREAMING_MODE_WANT_ALL as WantAll
  , FDB_STREAMING_MODE_EXACT as Exact
  } deriving (Eq, Ord, Show)
#}
