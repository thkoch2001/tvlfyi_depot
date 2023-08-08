module Seconds where

import Data.Aeson (FromJSON)
import Data.Aeson qualified as Json
import Data.Aeson.Types (FromJSON (parseJSON))
import Data.Scientific
import Data.Time (NominalDiffTime)
import FieldParser
import FieldParser qualified as Field
import GHC.Natural (naturalToInteger)
import PossehlAnalyticsPrelude

-- | A natural number of seconds.
newtype Seconds = Seconds {unSeconds :: Natural}
  deriving stock (Eq, Show)

-- | Parse a decimal number as a number of seconds
textToSeconds :: FieldParser Text Seconds
textToSeconds = Seconds <$> Field.decimalNatural

scientificToSeconds :: FieldParser Scientific Seconds
scientificToSeconds =
  ( Field.boundedScientificIntegral @Int "Number of seconds"
      >>> Field.integralToNatural
  )
    & rmap Seconds

-- Microseconds, represented internally with a 64 bit Int
newtype MicrosecondsInt = MicrosecondsInt {unMicrosecondsInt :: Int}
  deriving stock (Eq, Show)

-- | Try to fit a number of seconds into a MicrosecondsInt
secondsToMicrosecondsInt :: FieldParser Seconds MicrosecondsInt
secondsToMicrosecondsInt =
  lmap
    (\sec -> naturalToInteger sec.unSeconds * 1_000_000)
    (Field.bounded "Could not fit into an Int after multiplying with 1_000_000 (seconds to microseconds)")
    & rmap MicrosecondsInt

secondsToNominalDiffTime :: Seconds -> NominalDiffTime
secondsToNominalDiffTime sec =
  sec.unSeconds
    & naturalToInteger
    & fromInteger @NominalDiffTime

instance FromJSON Seconds where
  parseJSON = Field.toParseJSON jsonNumberToSeconds

-- | Parse a json number as a number of seconds.
jsonNumberToSeconds :: FieldParser' Error Json.Value Seconds
jsonNumberToSeconds = Field.jsonNumber >>> scientificToSeconds

-- | Return the number of seconds in a week
secondsInAWeek :: Seconds
secondsInAWeek = Seconds (3600 * 24 * 7)
