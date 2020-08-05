--------------------------------------------------------------------------------
module Spec where
--------------------------------------------------------------------------------
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Keyboard (Keyboard(..))
import Transforms (Transform(..))

import qualified App
import qualified Keyboard
import qualified Transforms
import qualified Utils
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

  describe "App.transform" $ do
    it "flips a keyboard horizontally" $ do
      App.transform Keyboard.qwerty HorizontalFlip == do
        Keyboard [ reverse ['1','2','3','4','5','6','7','8','9','0']
                 , reverse ['Q','W','E','R','T','Y','U','I','O','P']
                 , reverse ['A','S','D','F','G','H','J','K','L',';']
                 , reverse ['Z','X','C','V','B','N','M',',','.','/']
                 ]

    it "flips a keyboard vertically" $ do
      App.transform Keyboard.qwerty VerticalFlip == do
        Keyboard $ reverse [ ['1','2','3','4','5','6','7','8','9','0']
                           , ['Q','W','E','R','T','Y','U','I','O','P']
                           , ['A','S','D','F','G','H','J','K','L',';']
                           , ['Z','X','C','V','B','N','M',',','.','/']
                           ]

    it "shifts a keyboard N times" $ do
      App.transform Keyboard.qwerty (Shift 2) == do
        Keyboard $ [ Utils.rotate 2 ['1','2','3','4','5','6','7','8','9','0']
                   , Utils.rotate 2 ['Q','W','E','R','T','Y','U','I','O','P']
                   , Utils.rotate 2 ['A','S','D','F','G','H','J','K','L',';']
                   , Utils.rotate 2 ['Z','X','C','V','B','N','M',',','.','/']
                   ]

    it "shifts negative amounts" $ do
      App.transform Keyboard.qwerty (Shift (-3)) == do
        Keyboard $ [ Utils.rotate (-3) ['1','2','3','4','5','6','7','8','9','0']
                   , Utils.rotate (-3) ['Q','W','E','R','T','Y','U','I','O','P']
                   , Utils.rotate (-3) ['A','S','D','F','G','H','J','K','L',';']
                   , Utils.rotate (-3) ['Z','X','C','V','B','N','M',',','.','/']
                   ]
