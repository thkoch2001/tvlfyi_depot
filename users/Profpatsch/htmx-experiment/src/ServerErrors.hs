{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module ServerErrors where

import Control.Exception (Exception)
import Control.Monad.Logger (MonadLogger, logError, logWarn)
import Data.ByteString.Lazy qualified as Bytes.Lazy
import Data.Error.Tree
import Network.HTTP.Types qualified as Http
import PossehlAnalyticsPrelude

data ServerError = ServerError
  { status :: Http.Status,
    errBody :: Bytes.Lazy.ByteString
  }
  deriving stock (Show)
  deriving anyclass (Exception)

emptyServerError :: Http.Status -> ServerError
emptyServerError status = ServerError {status, errBody = ""}

-- | Throw a user error.
--
-- “User” here is a client using our API, not a human user.
-- So we throw a `HTTP 400` error, which means the API was used incorrectly.
--
-- We also log the error as a warning, because it probably signifies a programming bug in our client.
--
-- If you need to display a message to a human user, return a `FrontendResponse`
-- or a structured type with translation keys (so we can localize the errors).
throwUserError ::
  (MonadLogger m, MonadThrow m) =>
  -- | The error to log & throw to the user
  Error ->
  m b
throwUserError err = do
  -- TODO: should we make this into a macro to keep the line numbers?
  $logWarn (err & errorContext "There was a “user holding it wrong” error, check the client code" & prettyError)
  throwM
    ServerError
      { status = Http.badRequest400,
        errBody = err & prettyError & textToBytesUtf8 & toLazyBytes
      }

-- | Throw a user error.
--
-- “User” here is a client using our API, not a human user.
-- So we throw a `HTTP 400` error, which means the API was used incorrectly.
--
-- We also log the error as a warning, because it probably signifies a programming bug in our client.
--
-- If you need to display a message to a human user, return a `FrontendResponse`
-- or a structured type with translation keys (so we can localize the errors).
throwUserErrorTree ::
  (MonadLogger m, MonadThrow m) =>
  -- | The error to log & throw to the user
  ErrorTree ->
  m b
throwUserErrorTree err = do
  -- TODO: should we make this into a macro to keep the line numbers?
  $logWarn (err & nestedError "There was a “user holding it wrong” error, check the client code" & prettyErrorTree)
  throwM
    ServerError
      { status = Http.badRequest400,
        errBody = err & prettyErrorTree & textToBytesUtf8 & toLazyBytes
      }

-- | Unwrap the `Either` and if `Left` throw a user error.
--
-- Intended to use in a pipeline, e.g.:
--
-- @@
-- doSomething
--   >>= orUserError "Oh no something did not work"
--   >>= doSomethingElse
-- @@
--
-- “User” here is a client using our API, not a human user.
-- So we throw a `HTTP 400` error, which means the API was used incorrectly.
--
-- We also log the error as a warning, because it probably signifies a programming bug in our client.
--
-- If you need to display a message to a human user, return a `FrontendResponse`
-- or a structured type with translation keys (so we can localize the errors).
orUserError ::
  (MonadThrow m, MonadLogger m) =>
  -- | The message to add as a context to the error being thrown
  Text ->
  -- | Result to unwrap and potentially throw
  Either Error a ->
  m a
orUserError outerMsg eErrA =
  orUserErrorTree outerMsg (first singleError eErrA)

-- | Unwrap the `Either` and if `Left` throw a user error. Will pretty-print the 'ErrorTree'
--
-- Intended to use in a pipeline, e.g.:
--
-- @@
-- doSomething
--   >>= orUserErrorTree "Oh no something did not work"
--   >>= doSomethingElse
-- @@
--
-- “User” here is a client using our API, not a human user.
-- So we throw a `HTTP 400` error, which means the API was used incorrectly.
--
-- We also log the error as a warning, because it probably signifies a programming bug in our client.
--
-- If you need to display a message to a human user, return a `FrontendResponse`
-- or a structured type with translation keys (so we can localize the errors).
orUserErrorTree ::
  (MonadThrow m, MonadLogger m) =>
  -- | The message to add as a context to the 'ErrorTree' being thrown
  Text ->
  -- | Result to unwrap and potentially throw
  Either ErrorTree a ->
  m a
orUserErrorTree outerMsg = \case
  Right a -> pure a
  Left err -> do
    -- TODO: this outer message should probably be added as a separate root instead of adding to the root error?
    let tree = errorTreeContext outerMsg err
    -- TODO: should we make this into a macro to keep the line numbers?
    $logWarn (errorTreeContext "There was a “user holding it wrong” error, check the client code" tree & prettyErrorTree)
    throwM
      ServerError
        { status = Http.badRequest400,
          errBody = tree & prettyErrorTree & textToBytesUtf8 & toLazyBytes
        }

-- | Throw an internal error.
--
-- “Internal” here means some assertion that we depend on failed,
-- e.g. some database request returned a wrong result/number of results
-- or some invariant that we expect to hold failed.
--
-- This prints the full error to the log,
-- and returns a “HTTP 500” error without the message.
--
-- If you want to signify a mishandling of the API (e.g. a wrong request), throw a `userError`.
-- If you need to display a message to a human user, return a `FrontendResponse`
-- or a structured type with translation keys (so we can localize the errors).
throwInternalError ::
  (MonadLogger m, MonadThrow m) =>
  -- | The error to log internally
  Error ->
  m b
throwInternalError err = do
  -- TODO: should we make this into a macro to keep the line numbers?
  $logError
    (err & prettyError)
  throwM $ emptyServerError Http.internalServerError500

-- | Throw an internal error.
--
-- “Internal” here means some assertion that we depend on failed,
-- e.g. some database request returned a wrong result/number of results
-- or some invariant that we expect to hold failed.
--
-- This prints the full error to the log,
-- and returns a “HTTP 500” error without the message.
--
-- If you want to signify a mishandling of the API (e.g. a wrong request), throw a `userError`.
-- If you need to display a message to a human user, return a `FrontendResponse`
-- or a structured type with translation keys (so we can localize the errors).
throwInternalErrorTree ::
  (MonadLogger m, MonadThrow m) =>
  -- | The error to log internally
  ErrorTree ->
  m b
throwInternalErrorTree err = do
  -- TODO: should we make this into a macro to keep the line numbers?
  $logError
    (err & prettyErrorTree)
  throwM $ emptyServerError   Http.internalServerError500

-- | Unwrap the `Either` and if `Left` throw an internal error.
--
-- Intended to use in a pipeline, e.g.:
--
-- @@
-- doSomething
--   >>= orInternalError "Oh no something did not work"
--   >>= doSomethingElse
-- @@
--
-- “Internal” here means some assertion that we depend on failed,
-- e.g. some database request returned a wrong result/number of results
-- or some invariant that we expect to hold failed.
--
-- This prints the full error to the log,
-- and returns a “HTTP 500” error without the message.
--
-- If you want to signify a mishandling of the API (e.g. a wrong request), throw a `userError`.
-- If you need to display a message to a human user, return a `FrontendResponse`
-- or a structured type with translation keys (so we can localize the errors).
orInternalError ::
  (MonadThrow m, MonadLogger m) =>
  -- | The message to add as a context to the error being thrown
  Text ->
  -- | Result to unwrap and potentially throw
  Either Error a ->
  m a
orInternalError outerMsg eErrA = orInternalErrorTree outerMsg (first singleError eErrA)

-- | Unwrap the `Either` and if `Left` throw an internal error. Will pretty-print the 'ErrorTree'.
--
-- Intended to use in a pipeline, e.g.:
--
-- @@
-- doSomething
--   >>= orInternalErrorTree "Oh no something did not work"
--   >>= doSomethingElse
-- @@
--
-- “Internal” here means some assertion that we depend on failed,
-- e.g. some database request returned a wrong result/number of results
-- or some invariant that we expect to hold failed.
--
-- This prints the full error to the log,
-- and returns a “HTTP 500” error without the message.
--
-- If you want to signify a mishandling of the API (e.g. a wrong request), throw a `userError`.
-- If you need to display a message to a human user, return a `FrontendResponse`
-- or a structured type with translation keys (so we can localize the errors).
orInternalErrorTree ::
  (MonadThrow m, MonadLogger m) =>
  -- | The message to add as a context to the 'ErrorTree' being thrown
  Text ->
  -- | Result to unwrap and potentially throw
  Either ErrorTree a ->
  m a
orInternalErrorTree outerMsg = \case
  Right a -> pure a
  Left err -> do
    -- TODO: this outer message should probably be added as a separate root instead of adding to the root error?
    let tree = errorTreeContext outerMsg err
    -- TODO: should we make this into a macro to keep the line numbers?
    $logError (tree & prettyErrorTree)
    throwM $ emptyServerError   Http.internalServerError500
