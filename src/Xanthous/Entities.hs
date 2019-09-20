{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
--------------------------------------------------------------------------------
module Xanthous.Entities
  ( Draw(..)
  , DrawCharacter(..)
  , DrawStyledCharacter(..)
  , DrawRawChar(..)
  , Entity(..)
  , SomeEntity(..)
  , downcastEntity
  , entityIs
  , _SomeEntity

  , Color(..)
  , KnownColor(..)

  , EntityChar(..)
  , HasChar(..)
  ) where
--------------------------------------------------------------------------------
import           Xanthous.Prelude hiding ((.=))
--------------------------------------------------------------------------------
import           Brick
import           Data.Typeable
import qualified Graphics.Vty.Attributes as Vty
import qualified Graphics.Vty.Image as Vty
import           Data.Aeson
import           Data.Generics.Product.Fields
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary.Generic
--------------------------------------------------------------------------------
import           Xanthous.Data
import           Xanthous.Orphans ()
--------------------------------------------------------------------------------

class (Show a, Eq a, Draw a) => Entity a where
  blocksVision :: a -> Bool
  description :: a -> Text

instance Entity a => Entity (Positioned a) where
  blocksVision (Positioned _ ent) = blocksVision ent
  description (Positioned _ ent) = description ent

--------------------------------------------------------------------------------
data SomeEntity where
  SomeEntity :: forall a. (Entity a, Typeable a) => a -> SomeEntity

instance Show SomeEntity where
  show (SomeEntity e) = "SomeEntity (" <> show e <> ")"

instance Eq SomeEntity where
  (SomeEntity (a :: ea)) == (SomeEntity (b :: eb)) = case eqT @ea @eb of
    Just Refl -> a == b
    _ -> False

instance Draw SomeEntity where
  drawWithNeighbors ns (SomeEntity ent) = drawWithNeighbors ns ent

instance Entity SomeEntity where
  blocksVision (SomeEntity ent) = blocksVision ent
  description (SomeEntity ent) = description ent

downcastEntity :: (Entity a, Typeable a) => SomeEntity -> Maybe a
downcastEntity (SomeEntity e) = cast e

entityIs :: forall a. (Entity a, Typeable a) => SomeEntity -> Bool
entityIs = isJust . downcastEntity @a

_SomeEntity :: forall a. (Entity a, Typeable a) => Prism' SomeEntity a
_SomeEntity = prism' SomeEntity downcastEntity

--------------------------------------------------------------------------------

class Draw a where
  drawWithNeighbors :: Neighbors (Vector SomeEntity) -> a -> Widget n
  drawWithNeighbors = const draw

  draw :: a -> Widget n
  draw = drawWithNeighbors $ pure mempty

instance Draw a => Draw (Positioned a) where
  drawWithNeighbors ns (Positioned _ a) = drawWithNeighbors ns a
  draw (Positioned _ a) = draw a

newtype DrawCharacter (char :: Symbol) (a :: Type) where
  DrawCharacter :: a -> DrawCharacter char a

instance KnownSymbol char => Draw (DrawCharacter char a) where
  draw _ = str $ symbolVal @char Proxy

--------------------------------------------------------------------------------

data Color = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White

class KnownColor (color :: Color) where
  colorVal :: forall proxy. proxy color -> Vty.Color

instance KnownColor 'Black where colorVal _ = Vty.black
instance KnownColor 'Red where colorVal _ = Vty.red
instance KnownColor 'Green where colorVal _ = Vty.green
instance KnownColor 'Yellow where colorVal _ = Vty.yellow
instance KnownColor 'Blue where colorVal _ = Vty.blue
instance KnownColor 'Magenta where colorVal _ = Vty.magenta
instance KnownColor 'Cyan where colorVal _ = Vty.cyan
instance KnownColor 'White where colorVal _ = Vty.white

newtype DrawStyledCharacter (fg :: Color) (bg :: Color) (char :: Symbol) (a :: Type) where
  DrawStyledCharacter :: a -> DrawStyledCharacter fg bg char a

instance
  ( KnownColor fg
  , KnownColor bg
  , KnownSymbol char
  )
  => Draw (DrawStyledCharacter fg bg char a) where
  draw _ = raw $ Vty.string attr $ symbolVal @char Proxy
    where attr = Vty.Attr
            { Vty.attrStyle = Vty.Default
            , Vty.attrForeColor = Vty.SetTo $ colorVal @fg Proxy
            , Vty.attrBackColor = Vty.SetTo $ colorVal @bg Proxy
            , Vty.attrURL = Vty.Default
            }

--------------------------------------------------------------------------------

class HasChar s a | s -> a where
  char :: Lens' s a
  {-# MINIMAL char #-}

newtype DrawRawChar (rawField :: Symbol) (a :: Type) = DrawRawChar a

instance
  forall rawField a raw.
  ( HasField rawField a a raw raw
  , HasChar raw EntityChar
  ) => Draw (DrawRawChar rawField a) where
  draw (DrawRawChar e) = draw $ e ^. field @rawField . char

--------------------------------------------------------------------------------

data EntityChar = EntityChar
  { _char :: Char
  , _style :: Vty.Attr
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData, CoArbitrary, Function)

instance Arbitrary EntityChar where
  arbitrary = genericArbitrary

instance FromJSON EntityChar where
  parseJSON (String (chr :< Empty)) = pure $ EntityChar chr Vty.defAttr
  parseJSON (Object o) = do
    (EntityChar _char _) <- o .: "char"
    _style <- o .:? "style" >>= \case
      Just styleO -> do
        let attrStyle = Vty.Default -- TODO
            attrURL = Vty.Default
        attrForeColor <- styleO .:? "foreground" .!= Vty.Default
        attrBackColor <- styleO .:? "background" .!= Vty.Default
        pure Vty.Attr {..}
      Nothing -> pure Vty.defAttr
    pure EntityChar {..}
  parseJSON _ = fail "Invalid type, expected string or object"

instance ToJSON EntityChar where
  toJSON (EntityChar chr styl)
    | styl == Vty.defAttr = String $ chr <| Empty
    | otherwise = object
      [ "char" .= chr
      , "style" .= object
        [ "foreground" .= Vty.attrForeColor styl
        , "background" .= Vty.attrBackColor styl
        ]
      ]

instance Draw EntityChar where
  draw EntityChar{..} = raw $ Vty.string _style [_char]
