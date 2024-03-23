module Optional where

import GHC.Records (getField)
import MyPrelude

newtype Optional a = OptionalInternal (Maybe a)
  deriving newtype (Functor)

mkOptional :: a -> Optional a
mkOptional defaultValue = OptionalInternal $ Just defaultValue

defaults :: Optional a
defaults = OptionalInternal Nothing

instance HasField "withDefault" (Optional a) (a -> a) where
  getField (OptionalInternal m) defaultValue = case m of
    Nothing -> defaultValue
    Just a -> a
