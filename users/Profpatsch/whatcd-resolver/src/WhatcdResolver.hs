{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}

module WhatcdResolver where

import Control.Concurrent (threadDelay)
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
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple qualified as Postgres
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.Types (PGArray (PGArray))
import Database.PostgreSQL.Simple.Types qualified as Postgres
import Database.Postgres.Temp qualified as TmpPg
import FieldParser qualified as Field
import GHC.Records (HasField (..))
import IHP.HSX.QQ (hsx)
import Json qualified
import Json.Enc (Enc)
import Json.Enc qualified as Enc
import Label
import Network.HTTP.Conduit qualified as Http
import Network.HTTP.Simple qualified as Http
import Network.HTTP.Types
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import PossehlAnalyticsPrelude
import Postgres.Decoder qualified as Dec
import Postgres.MonadPostgres
import Pretty
import RunCommand
import System.Directory qualified as Dir
import System.Directory qualified as Xdg
import System.FilePath ((</>))
import System.IO qualified as IO
import Text.Blaze.Html ((!))
import Text.Blaze.Html.Renderer.Utf8 qualified as Html
import Text.Blaze.Html5 qualified as Html
import Text.Blaze.Html5.Attributes qualified as Attr
import UnliftIO

htmlUi :: App ()
htmlUi = do
  withRunInIO $ \runInIO -> Warp.run 8080 $ \req resp -> do
    let h = resp . Wai.responseLBS Http.ok200 []
    case req & Wai.pathInfo of
      [] -> h =<< runInIO mainHtml
      ["snips", "song"] -> h snipsSong
      _ -> h =<< runInIO mainHtml
  where
    mainHtml = runTransaction $ do
      tableData <-
        bestTorrents
          <&> Enc.list
            ( \t ->
                Enc.object
                  [ ("group_id", Enc.int t.groupId),
                    ("torrent_id", Enc.int t.torrentId),
                    ("artist", Enc.text t.torrentGroupJson.artist),
                    ("name", Enc.text t.torrentGroupJson.groupName),
                    ("weight", Enc.int t.seedingWeight),
                    ("torrent", Enc.value t.torrentJson)
                  ]
            )
          <&> Enc.encToBytesUtf8
          <&> bytesToTextUtf8Unsafe
          <&> Html.text
      let tableDataScript = Html.script ! Attr.type_ "application/json" ! Attr.id "table-data" $ tableData
      pure $
        Html.renderHtml $
          Html.docTypeHtml
            [hsx|
      <head>
        <meta charset="utf-8">
        <meta name="viewport" content="width=device-width, initial-scale=1">

        <script src="https://cdnjs.cloudflare.com/ajax/libs/jquery/3.7.0/jquery.min.js" integrity="sha512-3gJwYpMe3QewGELv8k/BX9vcqhryRdzRMxVfq6ngyWXwo03GFEzjsUm8Q7RZcHPHksttq7/GFoxjCVUjkjvPdw==" crossorigin="anonymous" referrerpolicy="no-referrer"></script>
        <link href="https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/css/bootstrap.min.css" rel="stylesheet" integrity="sha384-9ndCyUaIbzAi2FUVXJi0CjmCapSmO7SnpJef0486qhLnuZ2cdeRhO02iuK6FUUVM" crossorigin="anonymous">
<script src="https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/js/bootstrap.bundle.min.js" integrity="sha384-geWF76RCwLtnZ8qwWowPQNguL3RmwHVBC9FhGdlKrxdiJJigb/j/68SIy3Te4Bkz" crossorigin="anonymous"></script>
        <script src="https://cdnjs.cloudflare.com/ajax/libs/Dynatable/0.3.1/jquery.dynatable.min.js" integrity="sha512-KJdW8vGZWsRYrhMlZ6d8dR/fbLBK/aPOI0xDTEnGysk8TiFffc0S6TLSeSg7Lzk84GhBu9wI+qQatBrnTAeEYQ==" crossorigin="anonymous" referrerpolicy="no-referrer"></script>
        <script src="https://unpkg.com/htmx.org@1.9.2" integrity="sha384-L6OqL9pRWyyFU3+/bjdSri+iIphTN/bvYyM37tICVyOJkWZLpP2vGn6VUEXgzg6h" crossorigin="anonymous"></script>
        <script>
          $.dynatableSetup({
            table: {
              defaultColumnIdStyle: 'underscore'
            }
          });
        </script>
      </head>
      <body>
        {tableDataScript}
        <table id="table" class="table">
          <thead>
            <th>Group ID</th>
            <th>Torrent ID</th>
            <th>Artist</th>
            <th>Name</th>
            <th>Weight</th>
            <th>Torrent</th>
          </thead>
          <tbody>
          </tbody>
        </table>
        <script>
          var tableData = JSON.parse($("#table-data").text());
          $("table").dynatable({
            dataset: {
              records: tableData
            }
          } )
        </script>
      </body>
    |]
    snipsSong = todo

data TransmissionRequest = TransmissionRequest
  { method :: Text,
    arguments :: Map Text Enc,
    tag :: Maybe Int
  }
  deriving stock (Show)

testTransmission req = runAppWith $ doTransmissionRequest (T2 (label @"host" "localhost") (label @"port" "9091")) req >>= liftIO . printPretty

requestListAllTorrents :: TransmissionRequest
requestListAllTorrents =
  TransmissionRequest
    { method = "torrent-get",
      arguments =
        Map.fromList
          [ ("fields", Enc.list Enc.text ["id", "name", "files", "fileStats"])
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

redactedSearch ::
  (MonadLogger m, MonadIO m, MonadThrow m) =>
  [(ByteString, ByteString)] ->
  Json.Parse ErrorTree a ->
  m a
redactedSearch advanced =
  redactedApiRequest
    ( T2
        (label @"action" "browse")
        (label @"actionArgs" ((advanced <&> second Just)))
    )

test :: Bool -> IO (Either TmpPg.StartError ())
test doSearch =
  runAppWith $ do
    _ <- runTransaction migrate
    when doSearch $ do
      transaction <- bla
      _ <- runTransaction transaction
      pure ()
    htmlUi

-- fix
--   ( \io -> do
--       logInfo "delay"
--       liftIO $ threadDelay 10_000_000
--       io
--   )

bla :: (MonadThrow m, MonadIO m, MonadLogger m, MonadPostgres m) => m (Transaction m ())
bla = do
  t1 <-
    realbla
      [ ("searchstr", "cherish"),
        ("artistname", "kirinji"),
        -- ("year", "1982"),
        -- ("format", "MP3"),
        -- ("releasetype", "album"),
        ("order_by", "year")
      ]
  t2 <-
    realbla
      [ ("searchstr", "thriller"),
        ("artistname", "michael jackson"),
        -- ("year", "1982"),
        -- ("format", "MP3"),
        -- ("releasetype", "album"),
        ("order_by", "year")
      ]
  pure (t1 >> t2)
  where
    realbla x =
      redactedSearch
        x
        ( do
            status <- Json.key "status" Json.asText
            when (status /= "success") $ do
              Json.throwCustomError [fmt|Status was not "success", but {status}|]
            Json.key "response" $ do
              Json.key "results" $
                sequence_
                  <$> ( Json.eachInArray $ do
                          groupId <- Json.key "groupId" (Json.asIntegral @_ @Int)
                          groupName <- Json.key "groupName" Json.asText
                          fullJsonResult <-
                            Json.asObject
                              -- remove torrents cause they are inserted separately below
                              <&> KeyMap.filterWithKey (\k _ -> k /= "torrents")
                              <&> Json.Object
                          let insertTourGroup = do
                                _ <-
                                  execute
                                    [fmt|
                                  DELETE FROM redacted.torrent_groups
                                  WHERE group_id = ?::integer
                              |]
                                    (Only groupId)
                                executeManyReturningWith
                                  [fmt|
                                INSERT INTO redacted.torrent_groups (
                                  group_id, group_name, full_json_result
                                ) VALUES
                                ( ?, ? , ? )
                                RETURNING (id)
                              |]
                                  [ ( groupId,
                                      groupName,
                                      fullJsonResult
                                    )
                                  ]
                                  (label @"tourGroupIdPg" <$> Dec.fromField @Int)
                                  >>= ensureSingleRow
                          insertTorrents <- Json.key "torrents" $ do
                            torrents <- Json.eachInArray $ do
                              torrentId <- Json.keyLabel @"torrentId" "torrentId" (Json.asIntegral @_ @Int)
                              fullJsonResultT <- label @"fullJsonResult" <$> Json.asValue
                              pure $ T2 torrentId fullJsonResultT
                            pure $ \dat -> do
                              _ <-
                                execute
                                  [sql|
                                  DELETE FROM redacted.torrents_json
                                  WHERE torrent_id = ANY (?::integer[])
                            |]
                                  (Only $ torrents & unzipT2 & (.torrentId) & PGArray)
                              execute
                                [sql|
                                  INSERT INTO redacted.torrents_json
                                        (torrent_id, torrent_group, full_json_result)
                                  SELECT inputs.torrent_id, static.torrent_group, inputs.full_json_result FROM
                                  (SELECT * FROM UNNEST(?::integer[], ?::jsonb[])) AS inputs(torrent_id, full_json_result)
                                  CROSS JOIN (VALUES(?::integer)) as static(torrent_group)
                            |]
                                ( torrents
                                    & unzipT2
                                    & \t ->
                                      ( t.torrentId & PGArray,
                                        t.fullJsonResult & PGArray,
                                        dat.tourGroupIdPg
                                      )
                                )
                              pure ()
                          pure (insertTourGroup >>= insertTorrents)
                      )
        )

migrate :: MonadPostgres m => Transaction m (Label "numberOfRowsAffected" Natural)
migrate = do
  execute_
    [sql|
    CREATE SCHEMA IF NOT EXISTS redacted;

    CREATE TABLE IF NOT EXISTS redacted.torrent_groups (
      id SERIAL PRIMARY KEY,
      group_id INTEGER,
      group_name TEXT,
      full_json_result JSONB,
      UNIQUE(group_id)
    );

    CREATE TABLE IF NOT EXISTS redacted.torrents_json (
      id SERIAL PRIMARY KEY,
      torrent_id INTEGER,
      torrent_group SERIAL NOT NULL REFERENCES redacted.torrent_groups(id) ON DELETE CASCADE,
      full_json_result JSONB,
      UNIQUE(torrent_id)
    );

    -- inflect out values of the full json

    CREATE OR REPLACE VIEW redacted.torrents AS
    SELECT
      t.id,
      t.torrent_id,
      t.torrent_group,
      ( (full_json_result->'seeders')::integer*3
      + (full_json_result->'snatches')::integer)
      AS seeding_weight,
      t.full_json_result
    FROM redacted.torrents_json t;

    CREATE INDEX IF NOT EXISTS torrents_json_seeding ON redacted.torrents_json(((full_json_result->'seeding')::integer));
    CREATE INDEX IF NOT EXISTS torrents_json_snatches ON redacted.torrents_json(((full_json_result->'snatches')::integer));
  |]

data TorrentData = TorrentData
  { groupId :: Int,
    torrentId :: Int,
    seedingWeight :: Int,
    torrentJson :: Json.Value,
    torrentGroupJson :: T2 "artist" Text "groupName" Text
  }

bestTorrents :: MonadPostgres m => Transaction m [TorrentData]
bestTorrents = do
  queryWith
    [sql|
      SELECT * FROM (
        SELECT DISTINCT ON (group_id)
          tg.group_id,
          t.torrent_id,
          seeding_weight,
          t.full_json_result AS torrent_json,
          tg.full_json_result AS torrent_group_json
        FROM redacted.torrents t
        JOIN redacted.torrent_groups tg ON tg.id = t.torrent_group
        ORDER BY group_id, seeding_weight DESC
      ) as _
      ORDER BY seeding_weight DESC
    |]
    ()
    ( do
        groupId <- Dec.fromField @Int
        torrentId <- Dec.fromField @Int
        seedingWeight <- Dec.fromField @Int
        torrentJson <- Dec.json Json.asValue
        torrentGroupJson <-
          ( Dec.json $ do
              artist <- Json.keyLabel @"artist" "artist" Json.asText
              groupName <- Json.keyLabel @"groupName" "groupName" Json.asText
              pure $ T2 artist groupName
            )
        pure $ TorrentData {..}
    )

hush :: Either a1 a2 -> Maybe a2
hush (Left _) = Nothing
hush (Right a) = Just a

unzipT2 :: forall l1 t1 l2 t2. [T2 l1 t1 l2 t2] -> T2 l1 [t1] l2 [t2]
unzipT2 xs = xs <&> toTup & unzip & fromTup
  where
    toTup :: forall a b. T2 a t1 b t2 -> (t1, t2)
    toTup (T2 a b) = (getField @a a, getField @b b)
    fromTup :: (a, b) -> T2 l1 a l2 b
    fromTup (t1, t2) = T2 (label @l1 t1) (label @l2 t2)

unzipT3 :: forall l1 t1 l2 t2 l3 t3. [T3 l1 t1 l2 t2 l3 t3] -> T3 l1 [t1] l2 [t2] l3 [t3]
unzipT3 xs = xs <&> toTup & unzip3 & fromTup
  where
    toTup :: forall a b c. T3 a t1 b t2 c t3 -> (t1, t2, t3)
    toTup (T3 a b c) = (getField @a a, getField @b b, getField @c c)
    fromTup :: (a, b, c) -> T3 l1 a l2 b l3 c
    fromTup (t1, t2, t3) = T3 (label @l1 t1) (label @l2 t2) (label @l3 t3)

redactedApiRequest ::
  ( MonadThrow m,
    MonadIO m,
    MonadLogger m,
    HasField "action" p ByteString,
    HasField "actionArgs" p [(ByteString, Maybe ByteString)]
  ) =>
  p ->
  Json.Parse ErrorTree a ->
  m a
redactedApiRequest dat parse = do
  authKey <- runCommandExpect0 "pass" ["internet/redacted/api-keys/whatcd-resolver"]
  let req =
        [fmt|https://redacted.ch/ajax.php|]
          & Http.setRequestMethod "GET"
          & Http.setQueryString (("action", Just dat.action) : dat.actionArgs)
          & Http.setRequestHeader "Authorization" [authKey]
  Http.httpBS req
    >>= assertM
      ( \resp -> case resp & Http.responseStatus & (.statusCode) of
          200 -> Right $ resp & Http.responseBody
          _ -> Left [fmt|Redacted returned an non-200 error code: {resp & showPretty}|]
      )
    >>= ( Json.parseStrict parse
            >>> first (Json.parseErrorTree "could not parse redacted response")
            >>> assertM id
        )

assertM :: MonadThrow f => (t -> Either ErrorTree a) -> t -> f a
assertM f v = case f v of
  Right a -> pure a
  Left err -> appThrowTree err

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
  let newAppT = do
        logInfo [fmt|Running with config: {showPretty config}|]
        logInfo [fmt|Connected to database at {db & TmpPg.toDataDirectory} on socket {db & TmpPg.toConnectionString}|]
        appT
  runReaderT newAppT.unAppT Context {..}

withDb :: (TmpPg.DB -> IO a) -> IO (Either TmpPg.StartError a)
withDb act = do
  dataDir <- Xdg.getXdgDirectory Xdg.XdgData "whatcd-resolver"
  let databaseDir = dataDir </> "database"
  let socketDir = dataDir </> "database-socket"
  Dir.createDirectoryIfMissing True socketDir
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
            TmpPg.socketDirectory = TmpPg.Permanent socketDir,
            TmpPg.port = pure $ Just 5432,
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

type App a = AppT IO a

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
    logQueryIfEnabled conf qry (HasSingleParam params)
    pgExecute qry params
  execute_ qry = do
    conf <- lift $ AppT (asks (.config))
    logQueryIfEnabled @(Only Text) conf qry HasNoParams
    pgExecute_ qry
  executeMany qry params = do
    conf <- lift $ AppT (asks (.config))
    logQueryIfEnabled conf qry (HasMultiParams params)
    pgExecuteMany qry params
  executeManyReturningWith qry params dec = do
    conf <- lift $ AppT (asks (.config))
    logQueryIfEnabled conf qry (HasMultiParams params)
    pgExecuteManyReturningWith qry params dec

  queryWith qry params decoder = do
    conf <- lift $ AppT (asks (.config))
    logQueryIfEnabled conf qry (HasSingleParam params)
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

data HasQueryParams param
  = HasNoParams
  | HasSingleParam param
  | HasMultiParams [param]

-- | Log the postgres query depending on the setting of @config.debugInfo.logDatabaseQueries@.
logQueryIfEnabled ::
  forall params config m.
  ( Postgres.ToRow params,
    MonadUnliftIO m,
    MonadLogger m,
    MonadTools m,
    HasField "logDatabaseQueries" config DatabaseLogging
  ) =>
  config ->
  Postgres.Query ->
  HasQueryParams params ->
  Transaction m ()
logQueryIfEnabled config qry params = do
  -- In case we have query logging enabled, we want to do that
  let formattedQuery = case params of
        HasNoParams -> pgFormatQueryNoParams' qry
        HasSingleParam p -> pgFormatQuery' qry p
        HasMultiParams ps -> pgFormatQueryMany' qry ps

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
  deriving stock (Show)
