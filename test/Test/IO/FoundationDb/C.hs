{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module Test.IO.FoundationDb.C where

import qualified FoundationDb.C as C

import           Test.FoundationDb.Util (testIO)
import           Test.IO.FoundationDb.Process (withTestDb)
import           Test.QuickCheck

prop_true :: Property
prop_true =
  once $
    True
    ===
    True

prop_foo :: Property
prop_foo =
  once . testIO . withTestDb $ \_cluster database -> do
    txn <- C.createTransaction database
    C.transactionSet txn "foobar" "bazquux"
    ftr <- C.transactionGet txn "foobar" False
    C.futureBlockUntilReady ftr
    val <- C.futureGetValue ftr
    return $ val === pure "bazquux"

return []
tests :: IO Bool
tests = $quickCheckAll
