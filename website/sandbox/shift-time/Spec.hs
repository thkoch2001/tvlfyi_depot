--------------------------------------------------------------------------------
module Spec where
--------------------------------------------------------------------------------
import RIO
import Test.Hspec
import Test.QuickCheck
import Main hiding (main)

import qualified RIO.Text as Text
--------------------------------------------------------------------------------

main :: IO ()
main = hspec $ do
  describe "Main" $ do
    it "handles seconds" $ do
      property $ \x -> parseTime (Text.concat [x & show & Text.pack, "s"]) ==
        (Just defaultShiftTimeRequest { shiftSeconds = x })
