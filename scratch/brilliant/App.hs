--------------------------------------------------------------------------------
module App where
--------------------------------------------------------------------------------
import Keyboard (Keyboard(..))
import Transforms (Transform(..))
import Utils ((|>))

import qualified Utils
--------------------------------------------------------------------------------

transform :: Keyboard -> Transform -> Keyboard
transform (Keyboard xs) HorizontalFlip = xs |> fmap reverse |> Keyboard
transform (Keyboard xs) VerticalFlip   = xs |> reverse |> Keyboard
transform (Keyboard xs) (Shift n)      = xs |> fmap (Utils.rotate n) |> Keyboard
