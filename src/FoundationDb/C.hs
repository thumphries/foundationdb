{-# LANGUAGE ForeignFunctionInterface #-}
module FoundationDb.C (
    initialise
  , stop
  , Error (..)
  , catchError
  , errorMessage
  ) where


import           Control.Exception (Exception (..), throwIO, try)
import           Control.Monad (unless)

import           Data.Bifunctor (first)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B

import qualified FoundationDb.C.FFI as FFI
import           FoundationDb.C.Types

import           System.IO.Unsafe (unsafePerformIO)


initialise :: IO ()
initialise = do
  selectApiVersion
  setupNetwork
  runNetwork
  return ()

stop :: IO ()
stop =
  stopNetwork

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
-- Errors

data Error =
    Error !CError
  | SelectAPIError !CError
  | SetupNetworkError !CError
  | RunNetworkError !CError
  | StopNetworkError !CError
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
