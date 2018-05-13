{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module Test.IO.FoundationDb.C where

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
  once . testIO . withTestDb $ \_cluster _database -> do
    return ()

return []
tests :: IO Bool
tests = $quickCheckAll
