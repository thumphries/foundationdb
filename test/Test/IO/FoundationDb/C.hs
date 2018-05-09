{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module Test.IO.FoundationDb.C where

import           Test.QuickCheck

prop_true :: Property
prop_true =
  once $
    True
    ===
    True

return []
tests :: IO Bool
tests = $quickCheckAll
