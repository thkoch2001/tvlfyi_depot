module ValidationParseT where

import Control.Monad.Logger (MonadLogger)
import Control.Selective (Selective)
import Data.Error.Tree
import Data.Functor.Compose (Compose (..))
import PossehlAnalyticsPrelude
import ServerErrors

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

-- | Helper that runs the given parser and throws a user error if the parsing failed.
runValidationParseTOrUserError ::
  forall validationParseT env m a.
  ( Coercible validationParseT (ValidationParseT env m a),
    MonadLogger m,
    MonadThrow m
  ) =>
  -- | toplevel error message to throw if the parsing fails
  Error ->
  -- | The parser which should be run
  validationParseT ->
  -- | input to the parser
  env ->
  m a
{-# INLINE runValidationParseTOrUserError #-}
runValidationParseTOrUserError contextError parser env =
  (coerce @_ @(ValidationParseT _ _ _) parser).unValidationParseT env
    >>= \case
      Failure errs -> throwUserErrorTree (errorTree contextError errs)
      Success a -> pure a
