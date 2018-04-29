{-# LANGUAGE ForeignFunctionInterface #-}
module FoundationDb.C (
    errorMessage
  ) where


import           Data.ByteString (ByteString)
import qualified Data.ByteString as B

import           Foreign

import qualified FoundationDb.C.FFI as FFI
import           FoundationDb.C.Types

import           System.IO.Unsafe (unsafePerformIO)


errorMessage :: Error -> Maybe ByteString
errorMessage (Error cint) =
  unsafePerformIO $ do
    pt <- FFI.fdb_get_error cint
    if pt == nullPtr
      then return Nothing
      else Just <$> B.packCString pt
