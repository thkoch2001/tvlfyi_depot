--------------------------------------------------------------------------------
module Utils where
--------------------------------------------------------------------------------
import Data.Function ((&))
--------------------------------------------------------------------------------

-- | Prefer this operator to the ampersand for stylistic reasons.
(|>) :: a -> (a -> b) -> b
(|>) = (&)
