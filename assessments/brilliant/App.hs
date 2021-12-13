--------------------------------------------------------------------------------
module App where
--------------------------------------------------------------------------------
import Keyboard (Keyboard(..))
import Transforms (Transform(..))
import Utils ((|>))

import qualified Data.Char as Char
import qualified Utils
import qualified Data.List.Split as Split
import qualified Keyboard
import qualified Data.HashMap.Strict as HM
--------------------------------------------------------------------------------

transform :: Keyboard -> Transform -> Keyboard

transform (Keyboard xs) xform =
  case xform of
    HorizontalFlip ->
      xs
      |> fmap reverse
      |> Keyboard

    VerticalFlip ->
      xs
      |> reverse
      |> Keyboard

    Shift n ->
      xs
      |> concat
      |> Utils.rotate n
      |> Split.chunksOf 10
      |> Keyboard

retypePassage :: String -> Keyboard -> Maybe String
retypePassage passage newKeyboard =
  passage
  |> fmap Char.toUpper
  |> traverse (\c -> HM.lookup c Keyboard.charToCoord)
  >>= traverse (Keyboard.coordToChar newKeyboard)
