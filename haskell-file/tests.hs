module FTest where
--------------------------------------------------------------------------------
import Test.Tasty
import Test.Tasty.Hedgehog
import Hedgehog
--------------------------------------------------------------------------------
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
--------------------------------------------------------------------------------
import Data.List (intercalate)
import System.FilePath (pathSeparator)
--------------------------------------------------------------------------------
import F
--------------------------------------------------------------------------------
main :: IO ()
main
  = defaultMain
  . localOption (HedgehogTestLimit $ Just 50)
  $ testGroup "f functions"
  [ test_split
  ]
--------------------------------------------------------------------------------
test_split :: TestTree
test_split
  = testGroup "split function"
  [ testProperty "splits parts properly" splitSuccess
  ]
splitSuccess :: Property
splitSuccess = property $ do
  -- separator
  --   <- H.forAll
  --   $ Gen.element ['/', '\\']
  parts
    <- H.forAll
    . Gen.list (Range.linear 0 10)
    $ Gen.list (Range.linear 1 10) Gen.alphaNum
  let path = intercalate [pathSeparator] parts
  F.split path === parts
