{-# LANGUAGE ForeignFunctionInterface #-}
module FoundationDb.C.FFI (
    fdb_get_error
  , fdb_hs_select_api_version
  ) where


import           Foreign.C


foreign import ccall safe "foundationdb/fdb_c.h fdb_get_error"
  fdb_get_error :: CInt -> IO CString

foreign import ccall safe "fdb_hs.h fdb_hs_select_api_version"
  fdb_hs_select_api_version :: IO CInt
