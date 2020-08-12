--------------------------------------------------------------------------------
module Spec where
--------------------------------------------------------------------------------
import Test.Hspec
import Test.QuickCheck
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
    it "flips any keyboard horizontally" $ do
      property $ \first second third fourth ->
        App.transform (Keyboard [first, second, third, fourth]) HorizontalFlip == do
          Keyboard [ reverse first
                   , reverse second
                   , reverse third
                   , reverse fourth
                   ]

    it "flips any keyboard vertically" $ do
      property $ \first second third fourth ->
        App.transform (Keyboard [first, second, third, fourth]) VerticalFlip == do
          Keyboard $ reverse [first, second, third, fourth]

    it "shifts any keyboard" $ do
      property $ \first second third fourth n ->
        App.transform (Keyboard [first, second, third, fourth]) (Shift n) == do
          Keyboard $ [ Utils.rotate n first
                     , Utils.rotate n second
                     , Utils.rotate n third
                     , Utils.rotate n fourth
                     ]

    it "flips a QWERTY keyboard horizontally" $ do
      App.transform Keyboard.qwerty HorizontalFlip == do
        Keyboard [ ['0','9','8','7','6','5','4','3','2','1']
                 , ['P','O','I','U','Y','T','R','E','W','Q']
                 , [';','L','K','J','H','G','F','D','S','A']
                 , ['/','.',',','M','N','B','V','C','X','Z']
                 ]

    it "flips a keyboard vertically" $ do
      App.transform Keyboard.qwerty VerticalFlip == do
        Keyboard $ [ ['Z','X','C','V','B','N','M',',','.','/']
                   , ['A','S','D','F','G','H','J','K','L',';']
                   , ['Q','W','E','R','T','Y','U','I','O','P']
                   , ['1','2','3','4','5','6','7','8','9','0']
                   ]

    it "shifts a keyboard left N times" $ do
      App.transform Keyboard.qwerty (Shift 2) == do
        Keyboard $ [ ['3','4','5','6','7','8','9','0','1','2']
                   , ['E','R','T','Y','U','I','O','P','Q','W']
                   , ['D','F','G','H','J','K','L',';','A','S']
                   , ['C','V','B','N','M',',','.','/','Z','X']
                   ]

    it "shifts right negative amounts" $ do
      App.transform Keyboard.qwerty (Shift (-3)) == do
        Keyboard $ [ ['8','9','0','1','2','3','4','5','6','7']
                   , ['I','O','P','Q','W','E','R','T','Y','U']
                   , ['K','L',';','A','S','D','F','G','H','J']
                   , [',','.','/','Z','X','C','V','B','N','M']
                   ]

  describe "Transforms.optimize" $ do
    it "removes superfluous horizontal transformations" $ do
      Transforms.optimize [HorizontalFlip, HorizontalFlip] == []

    it "removes superfluous vertical transformations" $ do
      Transforms.optimize [VerticalFlip, VerticalFlip] == []
