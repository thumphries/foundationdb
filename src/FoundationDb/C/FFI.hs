{-# LANGUAGE ForeignFunctionInterface #-}
module FoundationDb.C.FFI (
    fdb_get_error
  , fdb_select_api_version
  ) where


import           Foreign
import           Foreign.C

import           FoundationDb.Types


foreign import ccall safe "fdb_c.h fdb_get_error"
  fdb_get_error :: CInt -> Ptr CString

foreign import ccall safe "fdb_c.h fdb_select_api_version"
  fdb_select_api_version :: CInt -> IO CInt
