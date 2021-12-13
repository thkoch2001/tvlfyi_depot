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

    it "handles minutes" $ do
      property $ \x -> parseTime (Text.concat [x & show & Text.pack, "m"]) ==
        (Just defaultShiftTimeRequest { shiftMinutes = x })

    it "handles hours" $ do
      property $ \x -> parseTime (Text.concat [x & show & Text.pack, "h"]) ==
        (Just defaultShiftTimeRequest { shiftHours = x })

    it "handles days" $ do
      property $ \x -> parseTime (Text.concat [x & show & Text.pack, "d"]) ==
        (Just defaultShiftTimeRequest { shiftDays = x })

    it "handles weeks" $ do
      property $ \x -> parseTime (Text.concat [x & show & Text.pack, "w"]) ==
        (Just defaultShiftTimeRequest { shiftWeeks = x })

    it "handles months" $ do
      property $ \x -> parseTime (Text.concat [x & show & Text.pack, "M"]) ==
        (Just defaultShiftTimeRequest { shiftMonths = x })

    it "handles quarters" $ do
      property $ \x -> parseTime (Text.concat [x & show & Text.pack, "q"]) ==
        (Just defaultShiftTimeRequest { shiftQuarters = x })

    it "handles multiple shifts" $ do
      parseTime "1s-20m5h0d-4w100M-3y2q" ==
        (Just $ ShiftTimeRequest
          { shiftSeconds = 1
          , shiftMinutes = -20
          , shiftHours = 5
          , shiftDays = 0
          , shiftWeeks = -4
          , shiftMonths = 100
          , shiftQuarters = 2
          , shiftYears = -3
          })
