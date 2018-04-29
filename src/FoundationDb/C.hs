{-# LANGUAGE ForeignFunctionInterface #-}
module FoundationDb.C (
    Error (..)
  , catchError
  , errorMessage

  -- * Setup / teardown
  , selectApiVersion
  , setupNetwork
  , runNetwork
  , stopNetwork

  -- * Futures
  , futureCancel
  , futureDestroy
  , futureBlockUntilReady
  , futureIsReady
  , futureSetCallback
  , futureReleaseMemory
  , futureGetError
  , futureGetKey
  , futureGetValue
  , futureGetStringArray
  , futureGetCluster
  , futureGetDatabase

  -- * Transactions
  , transactionDestroy
  , transactionGet
  , transactionGetKey
  , transactionSet
  , transactionCommit
  , transactionReset
  , transactionCancel
  ) where


import           Control.Exception (Exception (..), throwIO, try)
import           Control.Monad (unless)

import           Data.Bifunctor (first)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.Int (Int32)
import           Data.Vector (Vector)
import qualified Data.Vector.Storable as SV

import           Foreign.C.Types (CInt (..))
import           Foreign.ForeignPtr (newForeignPtr_)
import           Foreign.Marshal.Alloc (alloca)
import           Foreign.Storable (Storable (..))

import qualified FoundationDb.C.FFI as FFI
import           FoundationDb.C.Types

import           System.IO.Unsafe (unsafePerformIO)


-- ---------------------------------------------------------------------------
-- Setup / teardown of network thread

selectApiVersion :: IO ()
selectApiVersion =
  throwingX SelectAPIError $
    CError <$> FFI.fdb_hs_select_api_version

setupNetwork :: IO ()
setupNetwork =
  throwingX SetupNetworkError $
    CError <$> FFI.fdb_setup_network

runNetwork :: IO ()
runNetwork =
  throwingX RunNetworkError $
    CError <$> FFI.fdb_run_network

stopNetwork :: IO ()
stopNetwork =
  throwingX StopNetworkError $
    CError <$> FFI.fdb_stop_network

-- ---------------------------------------------------------------------------
-- Futures

futureCancel :: Future -> IO ()
futureCancel =
  FFI.fdb_future_cancel . unFuture

futureDestroy :: Future -> IO ()
futureDestroy =
  FFI.fdb_future_destroy . unFuture

futureBlockUntilReady :: Future -> IO ()
futureBlockUntilReady f =
  throwingX FutureBlockError $
    CError <$> FFI.fdb_future_block_until_ready (unFuture f)

futureIsReady :: Future -> IO Bool
futureIsReady f = do
  ret <- FFI.fdb_future_is_ready (unFuture f)
  return (cbool ret)

futureSetCallback :: Future -> Callback -> Param -> IO ()
futureSetCallback f c p = do
  throwingX FutureSetCallbackError $
    CError
      <$> FFI.fdb_future_set_callback
            (unFuture f) (unCallback c) (unParam p)

futureReleaseMemory :: Future -> IO ()
futureReleaseMemory f =
  FFI.fdb_future_release_memory (unFuture f)

futureGetError :: Future -> IO CError
futureGetError f =
  CError <$> FFI.fdb_future_get_error (unFuture f)

futureGetKey :: Future -> IO ByteString
futureGetKey f =
  alloca $ \keyPtrPtr ->
    alloca $ \lenPtr -> do
      throwingX FutureGetKeyError $
        CError <$> FFI.fdb_future_get_key (unFuture f) keyPtrPtr lenPtr
      len <- peek lenPtr
      keyPtr <- peek keyPtrPtr
      B.packCStringLen (keyPtr, fromIntegral len)

futureGetValue :: Future -> IO (Maybe ByteString)
futureGetValue f =
  alloca $ \boolPtr ->
    alloca $ \valPtrPtr ->
      alloca $ \lenPtr -> do
        throwingX FutureGetValueError $
          CError <$> FFI.fdb_future_get_value (unFuture f) boolPtr valPtrPtr lenPtr
        bool <- peek boolPtr
        case cbool bool of
          False ->
            return Nothing
          True -> do
            len <- peek lenPtr
            valPtr <- peek valPtrPtr
            Just <$> B.packCStringLen (valPtr, fromIntegral len)


futureGetStringArray :: Future -> IO (Vector ByteString)
futureGetStringArray f =
  alloca $ \arrPtr ->
    alloca $ \lenPtr -> do
      throwingX FutureGetStringArrayError $
        CError <$> FFI.fdb_future_get_string_array (unFuture f) arrPtr lenPtr
      len <- peek lenPtr
      arr <- peek arrPtr
      ptr <- newForeignPtr_ arr
      let vec = SV.convert $ SV.unsafeFromForeignPtr0 ptr (fromIntegral len)
      traverse B.packCString vec

futureGetCluster :: Future -> IO Cluster
futureGetCluster f =
  alloca $ \clsPtr -> do
    throwingX FutureGetClusterError $
      CError <$> FFI.fdb_future_get_cluster (unFuture f) clsPtr
    cls <- peek clsPtr
    return (Cluster cls)

futureGetDatabase :: Future -> IO Database
futureGetDatabase f =
  alloca $ \dbPtr -> do
    throwingX FutureGetDatabaseError $
      CError <$> FFI.fdb_future_get_database (unFuture f) dbPtr
    db <- peek dbPtr
    return (Database db)

-- ---------------------------------------------------------------------------
-- Transaction

transactionDestroy :: Transaction -> IO ()
transactionDestroy t =
  FFI.fdb_transaction_destroy (unTransaction t)

transactionGet :: Transaction -> ByteString -> Bool -> IO Future
transactionGet t key snapshot =
  B.useAsCStringLen key $ \(ptr, len) ->
    Future <$> FFI.fdb_transaction_get (unTransaction t) ptr len (boolc snapshot)

transactionGetKey :: Transaction -> ByteString -> Bool -> Int32 -> Bool -> IO Future
transactionGetKey t key orEqual offset snapshot =
  B.useAsCStringLen key $ \(ptr, len) ->
    fmap Future $
      FFI.fdb_transaction_get_key
        (unTransaction t)
        ptr
        len
        (boolc orEqual)
        (CInt offset)
        (boolc snapshot)

transactionSet :: Transaction -> ByteString -> ByteString -> IO ()
transactionSet t key value =
  B.useAsCStringLen key $ \(keyPtr, keyLen) ->
  B.useAsCStringLen value $ \(valuePtr, valueLen) ->
    FFI.fdb_transaction_set (unTransaction t) keyPtr keyLen valuePtr valueLen

transactionCommit :: Transaction -> IO Future
transactionCommit t =
  Future <$> FFI.fdb_transaction_commit (unTransaction t)

transactionReset :: Transaction -> IO ()
transactionReset t =
  FFI.fdb_transaction_reset (unTransaction t)

transactionCancel :: Transaction -> IO ()
transactionCancel t =
  FFI.fdb_transaction_cancel (unTransaction t)

-- ---------------------------------------------------------------------------
-- Errors

data Error =
    Error !CError
  | SelectAPIError !CError
  | SetupNetworkError !CError
  | RunNetworkError !CError
  | StopNetworkError !CError
  | FutureBlockError !CError
  | FutureSetCallbackError !CError
  | FutureGetKeyError !CError
  | FutureGetValueError !CError
  | FutureGetStringArrayError !CError
  | FutureGetClusterError !CError
  | FutureGetDatabaseError !CError
  deriving (Eq, Ord, Show)

-- | Produce a human-readable error message from a 'CError'.
errorMessage :: CError -> ByteString
errorMessage (CError cint) =
  unsafePerformIO $ do
    pt <- FFI.fdb_get_error cint
    B.packCString pt

-- ---------------------------------------------------------------------------
-- Exceptions

-- | To be caught at the outer level.
newtype FDBException = FDBException {
    unFDBException :: Error
  } deriving (Eq, Ord, Show)
instance Exception FDBException

throwX :: Error -> IO a
throwX e =
  throwIO (FDBException e)

throwingX :: (CError -> Error) -> IO CError -> IO ()
throwingX f x = do
  err <- x
  unless (csuccess err) $ throwX (f err)

catchError :: IO a -> IO (Either Error a)
catchError =
  fmap (first unFDBException) . try
