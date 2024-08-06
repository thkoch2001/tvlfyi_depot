{-# LANGUAGE DeriveAnyClass #-}

module AppT where

import Control.Monad.Logger qualified as Logger
import Control.Monad.Logger.CallStack
import Control.Monad.Reader
import Data.Error.Tree
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Pool (Pool)
import Data.Text qualified as Text
import Database.PostgreSQL.Simple qualified as Postgres
import GHC.Stack qualified
import Json.Enc
import Json.Enc qualified as Enc
import Label
import OpenTelemetry.Trace qualified as Otel hiding (getTracer, inSpan, inSpan')
import OpenTelemetry.Trace.Core qualified as Otel hiding (inSpan, inSpan')
import OpenTelemetry.Trace.Monad qualified as Otel
import PossehlAnalyticsPrelude
import Postgres.MonadPostgres
import System.IO qualified as IO
import UnliftIO
import Prelude hiding (span)

data Context = Context
  { pgConfig ::
      T2
        "logDatabaseQueries"
        DebugLogDatabaseQueries
        "prettyPrintDatabaseQueries"
        PrettyPrintDatabaseQueries,
    pgConnPool :: (Pool Postgres.Connection),
    tracer :: Otel.Tracer,
    transmissionSessionId :: IORef (Maybe ByteString),
    redactedApiKey :: ByteString
  }

newtype AppT m a = AppT {unAppT :: ReaderT Context m a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, MonadThrow)

newtype AppException = AppException Text
  deriving anyclass (Exception)

instance Show AppException where
  showsPrec _ (AppException t) = ("AppException: " ++) . (textToString t ++)

-- *  Logging & Opentelemetry

instance (MonadIO m) => MonadLogger (AppT m) where
  monadLoggerLog loc src lvl msg = liftIO $ Logger.defaultOutput IO.stderr loc src lvl (Logger.toLogStr msg)

instance (Monad m) => Otel.MonadTracer (AppT m) where
  getTracer = AppT $ asks (.tracer)

class (MonadUnliftIO m, Otel.MonadTracer m) => MonadOtel m

instance (MonadUnliftIO m) => MonadOtel (AppT m)

instance (MonadOtel m) => MonadOtel (Transaction m)

inSpan :: (MonadOtel m) => Text -> m a -> m a
inSpan name = Otel.inSpan name Otel.defaultSpanArguments

inSpan' :: (MonadOtel m) => Text -> (Otel.Span -> m a) -> m a
inSpan' name = Otel.inSpan' name Otel.defaultSpanArguments

-- | Add the attribute to the span, prefixing it with the `_` namespace (to easier distinguish our application’s tags from standard tags)
addAttribute :: (MonadIO m, Otel.ToAttribute a) => Otel.Span -> Text -> a -> m ()
addAttribute span key a = Otel.addAttribute span ("_." <> key) a

-- | Add the attributes to the span, prefixing each key with the `_` namespace (to easier distinguish our application’s tags from standard tags)
addAttributes :: (MonadIO m) => Otel.Span -> HashMap Text Otel.Attribute -> m ()
addAttributes span attrs = Otel.addAttributes span $ attrs & HashMap.mapKeys ("_." <>)

addEventSimple :: (MonadIO m) => Otel.Span -> Text -> m ()
addEventSimple span name =
  Otel.addEvent
    span
    Otel.NewEvent
      { Otel.newEventName = name,
        Otel.newEventTimestamp = Nothing,
        Otel.newEventAttributes = mempty
      }

-- | Create an otel attribute from a json encoder
jsonAttribute :: Enc -> Otel.Attribute
jsonAttribute e = e & Enc.encToTextPretty & Otel.toAttribute

orThrowAppErrorNewSpan :: (MonadThrow m, MonadOtel m) => Text -> Either ErrorTree a -> m a
orThrowAppErrorNewSpan msg = \case
  Left err -> appThrowTreeNewSpan msg err
  Right a -> pure a

appThrowTreeNewSpan :: (MonadThrow m, MonadOtel m) => Text -> ErrorTree -> m a
appThrowTreeNewSpan spanName exc = inSpan' spanName $ \span -> do
  let msg = prettyErrorTree exc
  recordException
    span
    ( T2
        (label @"type_" "AppException")
        (label @"message" msg)
    )
  throwM $ AppException msg

appThrowTree :: (MonadThrow m, MonadIO m) => Otel.Span -> ErrorTree -> m a
appThrowTree span exc = do
  let msg = prettyErrorTree exc
  recordException
    span
    ( T2
        (label @"type_" "AppException")
        (label @"message" msg)
    )
  throwM $ AppException msg

orAppThrowTree :: (MonadThrow m, MonadIO m) => Otel.Span -> Either ErrorTree a -> m a
orAppThrowTree span = \case
  Left err -> appThrowTree span err
  Right a -> pure a

assertM :: (MonadThrow f, MonadIO f) => Otel.Span -> (t -> Either ErrorTree a) -> t -> f a
assertM span f v = case f v of
  Right a -> pure a
  Left err -> appThrowTree span err

assertMNewSpan :: (MonadThrow f, MonadOtel f) => Text -> (t -> Either ErrorTree a) -> t -> f a
assertMNewSpan spanName f v = case f v of
  Right a -> pure a
  Left err -> appThrowTreeNewSpan spanName err

-- | A specialized variant of @addEvent@ that records attributes conforming to
-- the OpenTelemetry specification's
-- <https://github.com/open-telemetry/opentelemetry-specification/blob/49c2f56f3c0468ceb2b69518bcadadd96e0a5a8b/specification/trace/semantic_conventions/exceptions.md semantic conventions>
--
-- @since 0.0.1.0
recordException ::
  ( MonadIO m,
    HasField "message" r Text,
    HasField "type_" r Text
  ) =>
  Otel.Span ->
  r ->
  m ()
recordException span dat = liftIO $ do
  callStack <- GHC.Stack.whoCreated dat.message
  newEventTimestamp <- Just <$> Otel.getTimestamp
  Otel.addEvent span $
    Otel.NewEvent
      { newEventName = "exception",
        newEventAttributes =
          HashMap.fromList
            [ ("exception.type", Otel.toAttribute @Text dat.type_),
              ("exception.message", Otel.toAttribute @Text dat.message),
              ("exception.stacktrace", Otel.toAttribute @Text $ Text.unlines $ Prelude.map stringToText callStack)
            ],
        ..
      }

-- * Postgres

instance (MonadThrow m, MonadUnliftIO m) => MonadPostgres (AppT m) where
  execute = executeImpl dbConfig
  executeMany = executeManyImpl dbConfig
  executeManyReturningWith = executeManyReturningWithImpl dbConfig
  queryWith = queryWithImpl dbConfig
  queryWith_ = queryWithImpl_ (dbConfig <&> snd)

  foldRowsWithAcc = foldRowsWithAccImpl dbConfig
  runTransaction = runPGTransaction

dbConfig :: (Monad m) => AppT m (DebugLogDatabaseQueries, PrettyPrintDatabaseQueries)
dbConfig =
  AppT $
    asks
      ( \c ->
          ( c.pgConfig.logDatabaseQueries,
            c.pgConfig.prettyPrintDatabaseQueries
          )
      )

runPGTransaction :: (MonadUnliftIO m) => Transaction (AppT m) a -> AppT m a
runPGTransaction (Transaction transaction) = do
  pool <- AppT ask <&> (.pgConnPool)
  withRunInIO $ \unliftIO ->
    withPGTransaction pool $ \conn -> do
      unliftIO $ runReaderT transaction conn
