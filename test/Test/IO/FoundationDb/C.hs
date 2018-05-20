{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module Test.IO.FoundationDb.C where

import qualified FoundationDb.C as C

import           Test.FoundationDb.Util (testIO)
import           Test.IO.FoundationDb.Process (withTestDb)
import           Test.QuickCheck


prop_foo :: Property
prop_foo =
  once . testIO . withTestDb $ \_cluster database -> do
    txn <- C.createTransaction database
    C.transactionSet txn "foobar" "bazquux"
    ftr <- C.transactionGet txn "foobar" False
    C.futureBlockUntilReady ftr
    val <- C.futureGetValue ftr
    C.futureDestroy ftr
    C.transactionDestroy txn
    return $ val === pure "bazquux"

prop_bar :: Property
prop_bar =
  once . testIO . withTestDb $ \_cluster database -> do
    txn <- C.createTransaction database
    C.transactionSet txn "foobar" "bazquux"
    ftr <- C.transactionGet txn "zoox" False
    C.futureBlockUntilReady ftr
    val <- C.futureGetValue ftr
    C.futureDestroy ftr
    C.transactionDestroy txn
    return $ val === Nothing

return []
tests :: IO Bool
tests = $quickCheckAll
