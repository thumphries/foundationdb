module Test.FoundationDb.Util (
    testIO
  ) where


import           Test.QuickCheck
import qualified Test.QuickCheck.Monadic as QCM


testIO :: Testable prop => IO prop -> Property
testIO k =
    QCM.monadicIO $ do
        p <- QCM.run k
        _ <- QCM.stop p
        pure p
