--------------------------------------------------------------------------------
module App where
--------------------------------------------------------------------------------
import Keyboard (Keyboard(..))
import Transforms (Transform(..))
import Utils ((|>))

import qualified Utils
--------------------------------------------------------------------------------

transform :: Transform -> Keyboard -> Keyboard
transform HorizontalFlip (Keyboard xs) = xs |> fmap reverse |> Keyboard
transform VerticalFlip (Keyboard xs) = xs |> reverse |> Keyboard
transform (Shift n) (Keyboard xs) = xs |> fmap (Utils.rotate n) |> Keyboard
