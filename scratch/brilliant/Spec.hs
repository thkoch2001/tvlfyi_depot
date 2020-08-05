--------------------------------------------------------------------------------
module Spec where
--------------------------------------------------------------------------------
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Transforms (Transform(..))

import qualified Keyboard
import qualified Transforms
--------------------------------------------------------------------------------

main :: IO ()
main = hspec $ do
  describe "Keyboard.print" $ do
    it "pretty-prints the keyboard" $ do
      show Keyboard.qwerty == "[1][2][3][4][5][6][7][8][9][0]\n[Q][W][E][R][T][Y][U][I][O][P]\n[A][S][D][F][G][H][J][K][L][;]\n[Z][X][C][V][B][N][M][,][.][/]"

  describe "Transforms.fromString" $ do
    it "successfully parses a string of commands" $ do
      Transforms.fromString "HHVS-12VHVHS3" ==
        Just [ HorizontalFlip
             , HorizontalFlip
             , VerticalFlip
             , Shift (-12)
             , VerticalFlip
             , HorizontalFlip
             , VerticalFlip
             , HorizontalFlip
             , Shift 3
             ]

    it "returns Nothing when the input is invalid" $ do
      Transforms.fromString "potato" == Nothing

    it "return Nothing when the input is valid except for the end" $ do
      Transforms.fromString "HVS10potato" == Nothing
