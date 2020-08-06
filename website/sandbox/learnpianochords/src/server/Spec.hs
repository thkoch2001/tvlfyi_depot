--------------------------------------------------------------------------------
module Spec where
--------------------------------------------------------------------------------
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
--------------------------------------------------------------------------------

main :: IO ()
main = hspec $ do
  describe "Testing" $ do
    it "is setup" $ do
      True == True
