--------------------------------------------------------------------------------
module App where
--------------------------------------------------------------------------------
import Keyboard (Keyboard(..))
import Transforms (Transform(..))
import Utils ((|>))

import qualified Data.Char as Char
import qualified Utils
import qualified Keyboard
import qualified Data.HashMap.Strict as HM
--------------------------------------------------------------------------------

transform :: Keyboard -> Transform -> Keyboard
transform (Keyboard xs) HorizontalFlip = xs |> fmap reverse |> Keyboard
transform (Keyboard xs) VerticalFlip   = xs |> reverse |> Keyboard
transform (Keyboard xs) (Shift n)      = xs |> fmap (Utils.rotate n) |> Keyboard

retypePassage :: String -> Keyboard -> Maybe String
retypePassage passage newKeyboard =
  passage
  |> fmap Char.toUpper
  |> traverse (\c -> HM.lookup c Keyboard.charToCoord)
  >>= traverse (Keyboard.coordToChar newKeyboard)
