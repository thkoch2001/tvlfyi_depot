{-# LANGUAGE QuasiQuotes #-}

module AtLeast where

import Data.Aeson (FromJSON (parseJSON))
import Data.Aeson.BetterErrors qualified as Json
import FieldParser (FieldParser)
import FieldParser qualified as Field
import GHC.Records (HasField (..))
import GHC.TypeLits (KnownNat, natVal)
import PossehlAnalyticsPrelude
  ( Natural,
    Proxy (Proxy),
    fmt,
    prettyError,
    (&),
  )

-- | A natural number that must be at least as big as the type literal.
newtype AtLeast (min :: Natural) num = AtLeast num
  -- Just use the instances of the wrapped number type
  deriving newtype (Eq, Show)

-- | This is the “destructor” for `AtLeast`, because of the phantom type (@min@) it cannot be inferred automatically.
instance HasField "unAtLeast" (AtLeast min num) num where
  getField (AtLeast num) = num

parseAtLeast ::
  forall min num.
  (KnownNat min, Integral num, Show num) =>
  FieldParser num (AtLeast min num)
parseAtLeast =
  let minInt = natVal (Proxy @min)
   in Field.FieldParser $ \from ->
        if from >= (minInt & fromIntegral)
          then Right (AtLeast from)
          else Left [fmt|Must be at least {minInt & show} but was {from & show}|]

instance
  (KnownNat min, FromJSON num, Integral num, Bounded num, Show num) =>
  FromJSON (AtLeast min num)
  where
  parseJSON =
    Json.toAesonParser
      prettyError
      ( do
          num <- Json.fromAesonParser @_ @num
          case Field.runFieldParser (parseAtLeast @min @num) num of
            Left err -> Json.throwCustomError err
            Right a -> pure a
      )
