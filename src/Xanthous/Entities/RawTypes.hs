{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
--------------------------------------------------------------------------------
module Xanthous.Entities.RawTypes
  ( CreatureType(..)
  , EdibleItem(..)
  , ItemType(..)
  , isEdible
  , EntityRaw(..)

    -- * Lens classes
  , HasName(..)
  , HasDescription(..)
  , HasLongDescription(..)
  , HasMaxHitpoints(..)
  , HasFriendly(..)
  , HasEatMessage(..)
  , HasHitpointsHealed(..)
  , HasEdible(..)
  , _Creature
  ) where
--------------------------------------------------------------------------------
import Xanthous.Prelude
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.Generic
import Data.Aeson.Generic.DerivingVia
import Data.Aeson (ToJSON, FromJSON)
--------------------------------------------------------------------------------
import Xanthous.Entities (EntityChar, HasChar(..))
import Xanthous.Messages (Message(..))
--------------------------------------------------------------------------------
data CreatureType = CreatureType
  { _name :: Text
  , _description :: Text
  , _char :: EntityChar
  , _maxHitpoints :: Word
  , _friendly :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)
  deriving (ToJSON, FromJSON)
       via WithOptions '[ FieldLabelModifier '[Drop 1] ]
                       CreatureType
makeFieldsNoPrefix ''CreatureType

instance Arbitrary CreatureType where
  arbitrary = genericArbitrary

--------------------------------------------------------------------------------

data EdibleItem = EdibleItem
  { _hitpointsHealed :: Int
  , _eatMessage :: Maybe Message
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData, CoArbitrary, Function)
  deriving (ToJSON, FromJSON)
       via WithOptions '[ FieldLabelModifier '[Drop 1] ]
                       EdibleItem
makeFieldsNoPrefix ''EdibleItem

instance Arbitrary EdibleItem where
  arbitrary = genericArbitrary

data ItemType = ItemType
  { _name            :: Text
  , _description     :: Text
  , _longDescription :: Text
  , _char            :: EntityChar
  , _edible          :: Maybe EdibleItem
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData, CoArbitrary, Function)
  deriving (ToJSON, FromJSON)
       via WithOptions '[ FieldLabelModifier '[Drop 1] ]
                       ItemType
makeFieldsNoPrefix ''ItemType

instance Arbitrary ItemType where
  arbitrary = genericArbitrary

isEdible :: ItemType -> Bool
isEdible = has $ edible . _Just

--------------------------------------------------------------------------------

data EntityRaw
  = Creature CreatureType
  | Item ItemType
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)
  deriving (FromJSON)
       via WithOptions '[ SumEnc ObjWithSingleField ]
                       EntityRaw
makePrisms ''EntityRaw

{-# ANN module ("HLint: ignore Use newtype instead of data" :: String) #-}
