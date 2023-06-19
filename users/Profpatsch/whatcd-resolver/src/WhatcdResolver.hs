{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}

module WhatcdResolver where

import Control.Monad.Logger qualified as Logger
import Control.Monad.Logger.CallStack
import Control.Monad.Reader
import Data.Aeson qualified as Json
import Data.Aeson.BetterErrors qualified as Json
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Error.Tree
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Pool (Pool)
import Data.Pool qualified as Pool
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Database.PostgreSQL.Simple qualified as Postgres
import Database.PostgreSQL.Simple.Types qualified as Postgres
import Database.Postgres.Temp qualified as TmpPg
import FieldParser qualified as Field
import Json qualified
import Json.Enc (Enc)
import Json.Enc qualified as Enc
import Label
import Network.HTTP.Conduit qualified as Http
import Network.HTTP.Simple qualified as Http
import Network.HTTP.Types
import PossehlAnalyticsPrelude
import Postgres.Decoder qualified as Dec
import Postgres.MonadPostgres
import Pretty
import RunCommand
import System.Directory qualified as Dir
import System.Directory qualified as Xdg
import System.FilePath ((</>))
import System.IO qualified as IO
import UnliftIO

data TransmissionRequest = TransmissionRequest
  { method :: Text,
    arguments :: Map Text Enc,
    tag :: Maybe Int
  }
  deriving stock (Show)

requestListAllTorrents =
  TransmissionRequest
    { method = "torrent-get",
      arguments =
        Map.fromList
          [ ("fields", Enc.list Enc.text ["id", "name"])
          ],
      tag = Nothing
    }

data TransmissionResponse = TransmissionResponse
  { result :: TransmissionResponseStatus,
    arguments :: Map Text Json.Value,
    tag :: Maybe Int
  }
  deriving stock (Show)

data TransmissionResponseStatus
  = TransmissionResponseSuccess
  | TransmissionResponseFailure Text
  deriving stock (Show)

doTransmissionRequest ::
  ( MonadIO m,
    MonadTransmission m,
    HasField "host" t1 Text,
    HasField "port" t1 Text,
    MonadThrow m
  ) =>
  t1 ->
  TransmissionRequest ->
  m TransmissionResponse
doTransmissionRequest dat req = do
  sessionId <- getTransmissionId
  let httpReq =
        [fmt|http://{dat.host}:{dat.port}/transmission/rpc|]
          & Http.setRequestMethod "POST"
          & Http.setRequestBodyLBS
            ( Enc.encToBytesUtf8Lazy $
                Enc.object
                  ( [ ("method", req.method & Enc.text),
                      ("arguments", Enc.map id req.arguments)
                    ]
                      <> (req.tag & maybe [] (\t -> [("tag", t & Enc.int)]))
                  )
            )
          & (sessionId & maybe id (Http.setRequestHeader "X-Transmission-Session-Id" . (: [])))
  resp <- Http.httpBS httpReq
  -- Implement the CSRF protection thingy
  case resp & Http.getResponseStatus & (.statusCode) of
    409 -> do
      tid <-
        resp
          & Http.getResponseHeader "X-Transmission-Session-Id"
          & nonEmpty
          & annotate [fmt|Missing "X-Transmission-Session-Id" header in 409 response: {showPretty resp}|]
          & unwrapIOError
          & liftIO
          <&> NonEmpty.head
      setTransmissionId tid
      doTransmissionRequest dat req
    200 ->
      resp
        & Http.getResponseBody
        & Json.parseStrict
          ( Json.mapError singleError $ do
              result <-
                Json.key "result" Json.asText <&> \case
                  "success" -> TransmissionResponseSuccess
                  err -> TransmissionResponseFailure err
              arguments <-
                Json.keyMay "arguments" Json.asObject
                  <&> fromMaybe mempty
                  <&> KeyMap.toMapText
              tag <-
                Json.keyMay
                  "tag"
                  (Field.jsonParser (Field.jsonNumber >>> Field.boundedScientificIntegral "tag too long"))
              pure TransmissionResponse {..}
          )
        & first (Json.parseErrorTree "Cannot parse transmission RPC response")
        & \case
          Right a -> pure a
          Left err -> appThrowTree err
    _ -> liftIO $ unwrapIOError $ Left [fmt|Non-200 response: {showPretty resp}|]

redactedSearch advanced = redactedApiRequest (T2 (label @"action" "browse") (label @"actionArgs" ((advanced <&> second Just))))

test =
  runAppWith $
    redactedSearch
      [ ("artistname", "michael jackson"),
        ("year", "1982"),
        ("format", "MP3"),
        ("releasetype", "album"),
        ("order_by", "year")
      ]
      <&> (fmap $ fromMaybe undefined)
      <&> (Http.getResponseBody)
      <&> showPrettyJson
      >>= liftIO . Text.putStrLn

redactedApiRequest dat = do
  authKey <- runCommandExpect0 "pass" ["internet/redacted/api-keys/whatcd-resolver"]
  let req =
        [fmt|https://redacted.ch/ajax.php|]
          & Http.setRequestMethod "GET"
          & Http.setQueryString (("action", Just dat.action) : dat.actionArgs)
          & Http.setRequestHeader "Authorization" [authKey]
  Http.httpBS req
    <&> fmap (Json.decodeStrict' @Json.Value)

runAppWith :: AppT IO a -> IO (Either TmpPg.StartError a)
runAppWith appT = withDb $ \db -> do
  tools <- initMonadTools (label @"envvar" "WHATCD_RESOLVER_TOOLS")
  let config = label @"logDatabaseQueries" LogDatabaseQueries
  pgConnPool <-
    Pool.createPool
      (Postgres.connectPostgreSQL (db & TmpPg.toConnectionString))
      Postgres.close
      {- number of stripes -} 5
      {- unusedResourceOpenTime -} 10
      {- max resources per stripe -} 10
  transmissionSessionId <- newEmptyMVar
  runReaderT appT.unAppT Context {..}

withDb :: (TmpPg.DB -> IO a) -> IO (Either TmpPg.StartError a)
withDb act = do
  dataDir <- Xdg.getXdgDirectory Xdg.XdgData "whatcd-resolver"
  let databaseDir = dataDir </> "database"
  initDbConfig <-
    Dir.doesDirectoryExist databaseDir >>= \case
      True -> pure TmpPg.Zlich
      False -> do
        putStderrLn [fmt|Database does not exist yet, creating in "{databaseDir}"|]
        Dir.createDirectoryIfMissing True databaseDir
        pure TmpPg.DontCare
  let cfg =
        mempty
          { TmpPg.dataDirectory = TmpPg.Permanent (databaseDir),
            TmpPg.initDbConfig
          }
  TmpPg.withConfig cfg $ \db -> do
    -- print [fmt|data dir: {db & TmpPg.toDataDirectory}|]
    -- print [fmt|conn string: {db & TmpPg.toConnectionString}|]
    act db

data Context = Context
  { config :: Label "logDatabaseQueries" DatabaseLogging,
    tools :: Tools,
    pgConnPool :: Pool Postgres.Connection,
    transmissionSessionId :: MVar ByteString
  }

newtype AppT m a = AppT {unAppT :: ReaderT Context m a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, MonadThrow)

data AppException = AppException Text
  deriving stock (Show)
  deriving anyclass (Exception)

appThrowTree :: MonadThrow m => ErrorTree -> m a
appThrowTree exc = throwM $ AppException $ prettyErrorTree exc

instance MonadIO m => MonadLogger (AppT m) where
  monadLoggerLog loc src lvl msg = liftIO $ Logger.defaultOutput IO.stderr loc src lvl (Logger.toLogStr msg)

instance Monad m => MonadTools (AppT m) where
  getTools = AppT $ asks (.tools)

class MonadTransmission m where
  getTransmissionId :: m (Maybe ByteString)
  setTransmissionId :: ByteString -> m ()

instance (MonadIO m) => MonadTransmission (AppT m) where
  getTransmissionId = AppT (asks (.transmissionSessionId)) >>= tryTakeMVar
  setTransmissionId t = do
    var <- AppT $ asks (.transmissionSessionId)
    putMVar var t

instance (MonadThrow m, MonadUnliftIO m) => MonadPostgres (AppT m) where
  execute qry params = do
    conf <- lift $ AppT (asks (.config))
    logQueryIfEnabled conf qry (Left params)
    pgExecute qry params
  executeMany qry params = do
    conf <- lift $ AppT (asks (.config))
    logQueryIfEnabled conf qry (Right params)
    pgExecuteMany qry params
  executeManyReturning qry params = do
    conf <- lift $ AppT (asks (.config))
    logQueryIfEnabled conf qry (Right params)
    pgExecuteManyReturning qry params

  queryWith qry params decoder = do
    conf <- lift $ AppT (asks (.config))
    logQueryIfEnabled conf qry (Left params)
    pgQueryWith qry params decoder

  -- TODO: log these queries as well with `logQueryIfEnabled`, but test out whether it works with query_ and foldRows first.
  queryWith_ = pgQueryWith_
  foldRows = pgFold

  runTransaction = runPGTransaction

runPGTransaction :: MonadUnliftIO m => Transaction (AppT m) a -> AppT m a
runPGTransaction (Transaction transaction) = do
  pool <- AppT ask <&> (.pgConnPool)
  withRunInIO $ \unliftIO ->
    withPGTransaction pool $ \conn -> do
      unliftIO $ runReaderT transaction conn

-- | Perform a Postgres action within a transaction
withPGTransaction ::
  -- | Postgres connection pool to be used for the action
  Pool Postgres.Connection ->
  -- | DB-action to be performed
  (Postgres.Connection -> IO a) ->
  -- | Result of the DB-action
  IO a
withPGTransaction connPool f =
  Pool.withResource
    connPool
    (\conn -> Postgres.withTransaction conn (f conn))

-- | Log the postgres query depending on the setting of @config.debugInfo.logDatabaseQueries@.
logQueryIfEnabled ::
  ( Postgres.ToRow params,
    MonadUnliftIO m,
    MonadLogger m,
    MonadTools m,
    HasField "logDatabaseQueries" config DatabaseLogging
  ) =>
  config ->
  Postgres.Query ->
  Either params [params] ->
  Transaction m ()
logQueryIfEnabled config qry params = do
  -- In case we have query logging enabled, we want to do that
  let formattedQuery = case params of
        Left p -> pgFormatQuery' qry p
        Right ps -> pgFormatQueryMany' qry ps

  let doLog errs =
        errs
          & nestedMultiError "Postgres query"
          & prettyErrorTree
          & logDebug
          & lift
  let addQuery = do
        formattedQuery
          <&> newError
          <&> singleError
  let addExplain = do
        q <- formattedQuery
        pgQueryWith_
          ( "EXPLAIN "
              <> (
                   -- TODO: this is not nice, but the only way to get the `executeMany` form to work with this
                   -- because we need the query with all elements already interpolated.
                   Postgres.Query (q & textToBytesUtf8)
                 )
          )
          (Dec.fromField @Text)
          <&> Text.intercalate "\n"
          <&> newError
          <&> singleError

  case config.logDatabaseQueries of
    DontLogDatabaseQueries -> pure ()
    LogDatabaseQueries -> do
      aq <- addQuery
      doLog (aq :| [])
    LogDatabaseQueriesAndExplain -> do
      aq <- addQuery
      -- XXX: stuff like `CREATE SCHEMA` cannot be EXPLAINed, so we should catch exceptions here
      -- and just ignore anything that errors (if it errors because of a problem with the query, it would have been caught by the query itself.
      ex <- addExplain
      doLog (nestedError "Query" aq :| [nestedError "Explain" ex])

data DatabaseLogging
  = DontLogDatabaseQueries
  | LogDatabaseQueries
  | LogDatabaseQueriesAndExplain
