{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE DuplicateRecordFields #-}
--------------------------------------------------------------------------------
module Xanthous.Entities.RawTypes
  (
    EntityRaw(..)
  , _Creature
  , _Item

    -- * Creatures
  , CreatureType(..)
  , hostile
    -- ** Generation parameters
  , CreatureGenerateParams(..)
  , canGenerate
    -- ** Language
  , LanguageName(..)
  , getLanguage
    -- ** Attacks
  , Attack(..)

    -- * Items
  , ItemType(..)
    -- ** Item sub-types
    -- *** Edible
  , EdibleItem(..)
  , isEdible
    -- *** Wieldable
  , WieldableItem(..)
  , isWieldable

    -- * Lens classes
  , HasAttacks(..)
  , HasAttackMessage(..)
  , HasChar(..)
  , HasDamage(..)
  , HasDensity(..)
  , HasDescription(..)
  , HasEatMessage(..)
  , HasEdible(..)
  , HasFriendly(..)
  , HasGenerateParams(..)
  , HasHitpointsHealed(..)
  , HasLanguage(..)
  , HasLongDescription(..)
  , HasMaxHitpoints(..)
  , HasMaxLevel(..)
  , HasMinLevel(..)
  , HasName(..)
  , HasSayVerb(..)
  , HasSpeed(..)
  , HasVolume(..)
  , HasWieldable(..)
  ) where
--------------------------------------------------------------------------------
import Xanthous.Prelude
import Test.QuickCheck
import Data.Aeson.Generic.DerivingVia
import Data.Aeson (ToJSON, FromJSON)
import Data.Interval (Interval, lowerBound', upperBound')
--------------------------------------------------------------------------------
import Xanthous.Messages (Message(..))
import Xanthous.Data (TicksPerTile, Hitpoints, Per, Grams, Cubic, Meters)
import Xanthous.Data.EntityChar
import Xanthous.Util.QuickCheck
import Xanthous.Generators.Speech (Language, gormlak, english)
import Xanthous.Orphans ()
import Xanthous.Util (EqProp, EqEqProp(..))
--------------------------------------------------------------------------------

-- | Identifiers for languages that creatures can speak.
--
-- Non-verbal or non-sentient creatures have Nothing as their language
--
-- At some point, we will likely want to make languages be defined in data files
-- somewhere, and reference them that way instead.
data LanguageName = Gormlak | English
  deriving stock (Show, Eq, Ord, Generic, Enum, Bounded)
  deriving anyclass (NFData, CoArbitrary, Function)
  deriving Arbitrary via GenericArbitrary LanguageName
  deriving (ToJSON, FromJSON)
       via WithOptions '[ AllNullaryToStringTag 'True ]
                       LanguageName

-- | Resolve a 'LanguageName' into an actual 'Language'
getLanguage :: LanguageName -> Language
getLanguage Gormlak = gormlak
getLanguage English = english

-- | Natural attacks for creature types
data Attack = Attack
  { -- | the @{{creature}}@ @{{description}}@
    _description :: !Message
    -- | Damage dealt
  , _damage      :: !Hitpoints
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, CoArbitrary, Function)
  deriving Arbitrary via GenericArbitrary Attack
  deriving (ToJSON, FromJSON)
       via WithOptions '[ FieldLabelModifier '[Drop 1]
                        , OmitNothingFields 'True
                        ]
                       Attack
makeFieldsNoPrefix ''Attack

data CreatureGenerateParams = CreatureGenerateParams
  { -- | Minimum dungeon level at which to generate this creature
    _minLevel :: !(Maybe Word)
    -- | Maximum dungeon level at which to generate this creature
  , _maxLevel :: !(Maybe Word)
  }
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (NFData, CoArbitrary, Function)
  deriving Arbitrary via GenericArbitrary CreatureGenerateParams
  deriving EqProp via EqEqProp CreatureGenerateParams
  deriving (ToJSON, FromJSON)
       via WithOptions '[ FieldLabelModifier '[Drop 1] ]
                       CreatureGenerateParams
makeFieldsNoPrefix ''CreatureGenerateParams

-- | Can a creature with these generate params be generated on this level?
canGenerate
  :: Word -- ^ Level number
  -> CreatureGenerateParams
  -> Bool
canGenerate levelNumber gps = aboveLowerBound && belowUpperBound
  where
    aboveLowerBound = withinBound (>=) (gps ^. minLevel) levelNumber
    belowUpperBound = withinBound (<=) (gps ^. maxLevel) levelNumber
    withinBound cmp bound val = maybe True (cmp val) bound

instance Semigroup CreatureGenerateParams where
  (CreatureGenerateParams minl₁ maxl₁) <> (CreatureGenerateParams minl₂ maxl₂)
    = CreatureGenerateParams (addWith min minl₁ minl₂) (addWith max maxl₁ maxl₂)
    where
      addWith _ Nothing Nothing  = Nothing
      addWith _ Nothing (Just x)  = Just x
      addWith _ (Just x) Nothing  = Just x
      addWith f (Just x) (Just y) = Just (f x y)

instance Monoid CreatureGenerateParams where
  mempty = CreatureGenerateParams Nothing Nothing


data CreatureType = CreatureType
  { _name           :: !Text
  , _description    :: !Text
  , _char           :: !EntityChar
  , _maxHitpoints   :: !Hitpoints
  , _friendly       :: !Bool
  , _speed          :: !TicksPerTile
  , _language       :: !(Maybe LanguageName)
  , -- | The verb, in present tense, for when the creature says something
    _sayVerb        :: !(Maybe Text)
  , -- | The creature's natural attacks
    _attacks        :: !(NonNull (Vector Attack))
    -- | Parameters for generating the creature in levels
  , _generateParams :: !(Maybe CreatureGenerateParams)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, CoArbitrary, Function)
  deriving Arbitrary via GenericArbitrary CreatureType
  deriving (ToJSON, FromJSON)
       via WithOptions '[ FieldLabelModifier '[Drop 1]
                        , OmitNothingFields 'True
                        ]
                       CreatureType
makeFieldsNoPrefix ''CreatureType

hostile :: Lens' CreatureType Bool
hostile = friendly . involuted not

--------------------------------------------------------------------------------

data EdibleItem = EdibleItem
  { _hitpointsHealed :: !Int
  , _eatMessage      :: !(Maybe Message)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, CoArbitrary, Function)
  deriving Arbitrary via GenericArbitrary EdibleItem
  deriving (ToJSON, FromJSON)
       via WithOptions '[ FieldLabelModifier '[Drop 1] ]
                       EdibleItem
makeFieldsNoPrefix ''EdibleItem

data WieldableItem = WieldableItem
  { _damage :: !Hitpoints
  , _attackMessage :: !(Maybe Message)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, CoArbitrary, Function)
  deriving Arbitrary via GenericArbitrary WieldableItem
  deriving (ToJSON, FromJSON)
       via WithOptions '[ FieldLabelModifier '[Drop 1] ]
                       WieldableItem
makeFieldsNoPrefix ''WieldableItem

--------------------------------------------------------------------------------

data ItemType = ItemType
  { _name            :: !Text
  , _description     :: !Text
  , _longDescription :: !Text
  , _char            :: !EntityChar
  , _density         :: !(Interval (Grams `Per` Cubic Meters))
  , _volume          :: !(Interval (Cubic Meters))
  , _edible          :: !(Maybe EdibleItem)
  , _wieldable       :: !(Maybe WieldableItem)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData, CoArbitrary, Function)
  deriving Arbitrary via GenericArbitrary ItemType
  deriving (ToJSON, FromJSON)
       via WithOptions '[ FieldLabelModifier '[Drop 1] ]
                       ItemType
makeFieldsNoPrefix ''ItemType

instance Ord ItemType where
  compare x y
    = compareOf name x y
    <> compareOf description x y
    <> compareOf longDescription x y
    <> compareOf char x y
    <> compareOf (density . to extractInterval) x y
    <> compareOf (volume . to extractInterval) x y
    <> compareOf edible x y
    <> compareOf wieldable x y
    where
      compareOf l = comparing (view l)
      extractInterval = lowerBound' &&& upperBound'

-- | Can this item be eaten?
isEdible :: ItemType -> Bool
isEdible = has $ edible . _Just

-- | Can this item be used as a weapon?
isWieldable :: ItemType -> Bool
isWieldable = has $ wieldable . _Just

--------------------------------------------------------------------------------

data EntityRaw
  = Creature !CreatureType
  | Item !ItemType
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)
  deriving Arbitrary via GenericArbitrary EntityRaw
  deriving (FromJSON)
       via WithOptions '[ SumEnc ObjWithSingleField ]
                       EntityRaw
makePrisms ''EntityRaw
