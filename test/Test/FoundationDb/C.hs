{-# LANGUAGE TemplateHaskell #-}
module Test.FoundationDb.C where


import           Test.QuickCheck


prop_false :: Property
prop_false = True === False


return []
tests :: IO Bool
tests = $quickCheckAll
