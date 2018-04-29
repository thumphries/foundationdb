{-# LANGUAGE ForeignFunctionInterface #-}
module FoundationDb.C.FFI (
    fdb_get_error
  , fdb_hs_select_api_version
  , fdb_network_set_option
  , fdb_setup_network
  , fdb_run_network
  , fdb_stop_network
  , fdb_future_cancel
  , fdb_future_destroy
  , fdb_future_block_until_ready
  , fdb_future_is_ready
  , fdb_future_set_callback
  , fdb_future_release_memory
  , fdb_future_get_error
  , fdb_future_get_key
  , fdb_future_get_value
  , fdb_future_get_string_array
  , fdb_future_get_cluster
  , fdb_future_get_database
  , fdb_transaction_destroy
  , fdb_transaction_get
  , fdb_transaction_get_key
  , fdb_transaction_set
  , fdb_transaction_commit
  , fdb_transaction_reset
  , fdb_transaction_cancel
  ) where


import           Foreign
import           Foreign.C

import           FoundationDb.C.Types


foreign import ccall safe "fdb_hs.h fdb_hs_select_api_version"
  fdb_hs_select_api_version :: IO CInt

foreign import ccall safe "foundationdb/fdb_c.h fdb_get_error"
  fdb_get_error :: CInt -> IO CString

foreign import ccall safe "foundationdb/fdb_c.h fdb_network_set_option"
  fdb_network_set_option :: CInt -> CString -> CInt -> IO CInt

foreign import ccall safe "foundationdb/fdb_c.h fdb_setup_network"
  fdb_setup_network :: IO CInt

foreign import ccall safe "foundationdb/fdb_c.h fdb_run_network"
  fdb_run_network :: IO CInt

foreign import ccall safe "foundationdb/fdb_c.h fdb_stop_network"
  fdb_stop_network :: IO CInt

foreign import ccall safe "foundationdb/fdb_c.h fdb_future_cancel"
  fdb_future_cancel :: Ptr Future' -> IO ()

foreign import ccall safe "foundationdb/fdb_c.h fdb_future_destroy"
  fdb_future_destroy :: Ptr Future' -> IO ()

foreign import ccall safe "foundationdb/fdb_c.h fdb_future_block_until_ready"
  fdb_future_block_until_ready :: Ptr Future' -> IO CInt

foreign import ccall safe "foundationdb/fdb_c.h fdb_future_is_ready"
  fdb_future_is_ready :: Ptr Future' -> IO CInt

foreign import ccall safe "foundationdb/fdb_c.h fdb_future_set_callback"
  fdb_future_set_callback :: Ptr Future' -> FunPtr (Ptr Future' -> Ptr Param' -> IO ()) -> Ptr Param' -> IO CInt

foreign import ccall safe "foundationdb/fdb_c.h fdb_future_release_memory"
  fdb_future_release_memory :: Ptr Future' -> IO ()

foreign import ccall safe "foundationdb/fdb_c.h fdb_future_get_error"
  fdb_future_get_error :: Ptr Future' -> IO CInt

foreign import ccall safe "foundationdb/fdb_c.h fdb_future_get_key"
  fdb_future_get_key :: Ptr Future' -> Ptr CString -> Ptr CInt -> IO CInt

-- future_get_cluster
-- future_get_database

foreign import ccall safe "foundationdb/fdb_c.h fdb_future_get_value"
  fdb_future_get_value :: Ptr Future' -> Ptr CInt -> Ptr CString -> Ptr CInt -> IO CInt

foreign import ccall safe "foundationdb/fdb_c.h fdb_future_get_string_array"
  fdb_future_get_string_array :: Ptr Future' -> Ptr (Ptr CString) -> Ptr CInt -> IO CInt

foreign import ccall safe "foundationdb/fdb_c.h fdb_future_get_cluster"
  fdb_future_get_cluster :: Ptr Future' -> Ptr (Ptr Cluster') -> IO CInt

foreign import ccall safe "foundationdb/fdb_c.h fdb_future_get_database"
  fdb_future_get_database :: Ptr Future' -> Ptr (Ptr Database') -> IO CInt

foreign import ccall safe "foundationdb/fdb_c.h fdb_transaction_destroy"
  fdb_transaction_destroy :: Ptr Transaction' -> IO ()

foreign import ccall safe "foundationdb/fdb_c.h fdb_transaction_get"
  fdb_transaction_get :: Ptr Transaction' -> Ptr CChar -> Int -> CInt -> IO (Ptr Future')

foreign import ccall safe "foundationdb/fdb_c.h fdb_transaction_get_key"
  fdb_transaction_get_key :: Ptr Transaction' -> Ptr CChar -> Int -> CInt -> CInt -> CInt -> IO (Ptr Future')

foreign import ccall safe "foundationdb/fdb_c.h fdb_transaction_set"
  fdb_transaction_set :: Ptr Transaction' -> Ptr CChar -> Int -> Ptr CChar -> Int -> IO ()

foreign import ccall safe "foundationdb/fdb_c.h fdb_transaction_commit"
  fdb_transaction_commit :: Ptr Transaction' -> IO (Ptr Future')

foreign import ccall safe "foundationdb/fdb_c.h fdb_transaction_reset"
  fdb_transaction_reset :: Ptr Transaction' -> IO ()

foreign import ccall safe "foundationdb/fdb_c.h fdb_transaction_cancel"
  fdb_transaction_cancel :: Ptr Transaction' -> IO ()
