{-# LANGUAGE ForeignFunctionInterface #-}
module FoundationDb.C.FFI (
    fdb_get_error
  , fdb_network_set_option
  , fdb_setup_network
  , fdb_run_network
  , fdb_stop_network
  , fdb_hs_select_api_version
  ) where


import           Foreign.C


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

foreign import ccall safe "fdb_hs.h fdb_hs_select_api_version"
  fdb_hs_select_api_version :: IO CInt
