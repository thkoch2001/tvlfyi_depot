module ValidationParseT where

import Control.Selective (Selective)
import Data.Functor.Compose (Compose (..))
import PossehlAnalyticsPrelude

-- | A simple way to create an Applicative parser that parses from some environment.
--
-- Use with DerivingVia. Grep codebase for examples.
newtype ValidationParseT env m a = ValidationParseT {unValidationParseT :: env -> m (Validation (NonEmpty Error) a)}
  deriving
    (Functor, Applicative, Selective)
    via ( Compose
            ((->) env)
            (Compose m (Validation (NonEmpty Error)))
        )
