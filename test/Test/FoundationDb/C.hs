{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module Test.FoundationDb.C where


import           Foreign.C

import           FoundationDb.C
import           FoundationDb.C.Types

import           Test.QuickCheck


prop_errorMessage_noSegfault :: Property
prop_errorMessage_noSegfault =
  forAll (arbitrary @ CInt) $ \cint ->
    errorMessage (CError cint) `seq` property True

prop_errorMessage_1 :: Property
prop_errorMessage_1 =
  once $
    errorMessage (CError 1)
    ===
    "End of stream"


return []
tests :: IO Bool
tests = $quickCheckAll
