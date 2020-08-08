--------------------------------------------------------------------------------
module TestUtils where
--------------------------------------------------------------------------------
import Web.JWT
import Data.String.Conversions (cs)
--------------------------------------------------------------------------------

unsafeStringOrURI :: String -> StringOrURI
unsafeStringOrURI x =
  case stringOrURI (cs x) of
    Nothing -> error $ "Failed to convert to StringOrURI: " ++ x
    Just x  -> x

unsafeJust :: Maybe a -> a
unsafeJust Nothing = error "Attempted to force a Nothing to be a something"
unsafeJust (Just x) = x
