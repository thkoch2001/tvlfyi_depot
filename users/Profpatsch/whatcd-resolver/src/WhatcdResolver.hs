{-# LANGUAGE QuasiQuotes #-}

module WhatcdResolver where

import AppT
import Control.Category qualified as Cat
import Control.Monad.Catch.Pure (runCatch)
import Control.Monad.Logger.CallStack
import Control.Monad.Reader
import Data.Aeson qualified as Json
import Data.Aeson.BetterErrors qualified as Json
import Data.Aeson.KeyMap qualified as KeyMap
import Data.HashMap.Strict qualified as HashMap
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Pool qualified as Pool
import Data.Text qualified as Text
import Database.PostgreSQL.Simple qualified as Postgres
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.Types (PGArray (PGArray))
import Database.Postgres.Temp qualified as TmpPg
import FieldParser (FieldParser, FieldParser' (..))
import FieldParser qualified as Field
import Html qualified
import IHP.HSX.QQ (hsx)
import Json qualified
import Json.Enc (Enc)
import Json.Enc qualified as Enc
import JsonLd
import Label
import Multipart2 qualified as Multipart
import MyPrelude
import Network.HTTP.Client.Conduit qualified as Http
import Network.HTTP.Simple qualified as Http
import Network.HTTP.Types
import Network.HTTP.Types qualified as Http
import Network.URI (URI)
import Network.URI qualified
import Network.URI qualified as URI
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Parse qualified as Wai
import OpenTelemetry.Trace qualified as Otel hiding (getTracer, inSpan, inSpan')
import OpenTelemetry.Trace.Monad qualified as Otel
import Parse (Parse)
import Parse qualified
import Postgres.Decoder qualified as Dec
import Postgres.MonadPostgres
import Pretty
import Redacted
import System.Directory qualified as Dir
import System.Directory qualified as Xdg
import System.Environment qualified as Env
import System.FilePath ((</>))
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Pretty qualified as Html.Pretty
import Text.Blaze.Html.Renderer.Utf8 qualified as Html
import Text.Blaze.Html5 qualified as Html
import Tool (readTool, readTools)
import Transmission
import UnliftIO
import Prelude hiding (span)

main :: IO ()
main =
  runAppWith
    ( do
        -- todo: trace that to the init functions as well
        Otel.inSpan "whatcd-resolver main function" Otel.defaultSpanArguments $ do
          _ <- runTransaction migrate
          htmlUi
    )
    <&> first showToError
    >>= expectIOError "could not start whatcd-resolver"

htmlUi :: AppT IO ()
htmlUi = do
  let debug = True
  uniqueRunId <-
    runTransaction $
      querySingleRowWith
        [sql|
            SELECT gen_random_uuid()::text
        |]
        ()
        (Dec.fromField @Text)

  withRunInIO $ \runInIO -> Warp.run 9093 $ \req respond -> do
    let catchAppException act =
          try act >>= \case
            Right a -> pure a
            Left (AppException err) -> do
              runInIO (logError err)
              respond (Wai.responseLBS Http.status500 [] "")

    catchAppException $ do
      let renderHtml =
            if debug
              then Html.Pretty.renderHtml >>> stringToText >>> textToBytesUtf8 >>> toLazyBytes
              else Html.renderHtml
      let hh route act =
            runInIO $
              Otel.inSpan'
                [fmt|Route {route }|]
                ( Otel.defaultSpanArguments
                    { Otel.attributes =
                        HashMap.fromList
                          [ ("server.path", Otel.toAttribute @Text route)
                          ]
                    }
                )
                ( \span -> withRunInIO $ \runInIO' -> do
                    res <- runInIO' $ act span
                    respond . Wai.responseLBS Http.ok200 ([("Content-Type", "text/html")] <> res.extraHeaders) . renderHtml $ res.html
                )
      let h route act = hh route (\span -> act span <&> (\html -> T2 (label @"html" html) (label @"extraHeaders" [])))
      let mp span parser =
            Multipart.parseMultipartOrThrow
              (appThrowTree span)
              parser
              req

      let torrentIdMp span =
            mp
              span
              ( do
                  label @"torrentId" <$> Multipart.field "torrent-id" ((Field.utf8 >>> Field.signedDecimal >>> Field.bounded @Int "int"))
              )
      let parseQueryArgs span parser =
            Parse.runParse "Unable to find the right request query arguments" (lmap Wai.queryString parser) req
              & assertM span id

      let parseQueryArgsNewSpan spanName parser =
            Parse.runParse "Unable to find the right request query arguments" (lmap Wai.queryString parser) req
              & assertMNewSpan spanName id

      case req & Wai.pathInfo & Text.intercalate "/" of
        "" -> h "/" (mainHtml uniqueRunId)
        "snips/redacted/search" -> do
          h "/snips/redacted/search" $ \span -> do
            dat <-
              mp
                span
                ( do
                    label @"searchstr" <$> Multipart.field "redacted-search" Cat.id
                )
            snipsRedactedSearch dat
        "snips/redacted/torrentDataJson" -> h "/snips/redacted/torrentDataJson" $ \span -> do
          dat <- torrentIdMp span
          Html.mkVal <$> (runTransaction $ getTorrentById dat)
        "snips/redacted/getTorrentFile" -> h "/snips/redacted/getTorrentFile" $ \span -> do
          dat <- torrentIdMp span
          runTransaction $ do
            inserted <- redactedGetTorrentFileAndInsert dat
            running <-
              lift @Transaction $
                doTransmissionRequest' (transmissionRequestAddTorrent inserted)
            updateTransmissionTorrentHashById
              ( T2
                  (getLabel @"torrentHash" running)
                  (getLabel @"torrentId" dat)
              )
            pure $
              everySecond
                "snips/transmission/getTorrentState"
                (Enc.object [("torrent-hash", Enc.text running.torrentHash)])
                "Starting"
        -- TODO: this is bad duplication??
        "snips/redacted/startTorrentFile" -> h "/snips/redacted/startTorrentFile" $ \span -> do
          dat <- torrentIdMp span
          runTransaction $ do
            file <-
              getTorrentFileById dat
                <&> annotate [fmt|No torrent file for torrentId "{dat.torrentId}"|]
                >>= orAppThrowTree span

            running <-
              lift @Transaction $
                doTransmissionRequest' (transmissionRequestAddTorrent file)
            updateTransmissionTorrentHashById
              ( T2
                  (getLabel @"torrentHash" running)
                  (getLabel @"torrentId" dat)
              )
            pure $
              everySecond
                "snips/transmission/getTorrentState"
                (Enc.object [("torrent-hash", Enc.text running.torrentHash)])
                "Starting"
        "snips/transmission/getTorrentState" -> h "/snips/transmission/getTorrentState" $ \span -> do
          dat <- mp span $ label @"torrentHash" <$> Multipart.field "torrent-hash" Field.utf8
          status <-
            doTransmissionRequest'
              ( transmissionRequestListOnlyTorrents
                  ( T2
                      (label @"ids" [label @"torrentHash" dat.torrentHash])
                      (label @"fields" ["hashString"])
                  )
                  (Json.keyLabel @"torrentHash" "hashString" Json.asText)
              )
              <&> List.find (\torrent -> torrent.torrentHash == dat.torrentHash)

          pure $
            case status of
              Nothing -> [hsx|ERROR unknown|]
              Just _torrent -> [hsx|Running|]
        "snips/jsonld/render" ->
          h "/snips/jsonld/render" $ \span -> do
            qry <-
              parseQueryArgs
                span
                ( label @"target"
                    <$> ( (singleQueryArgument "target" Field.utf8 >>> textToURI)
                            & Parse.andParse uriToHttpClientRequest
                        )
                )
            jsonld <- httpGetJsonLd (qry.target)
            pure $ renderJsonld jsonld
        "autorefresh" -> do
          qry <-
            runInIO $
              parseQueryArgsNewSpan
                "Autorefresh Query Parse"
                ( label @"hasItBeenRestarted"
                    <$> singleQueryArgument "hasItBeenRestarted" Field.utf8
                )
          respond $
            Wai.responseLBS
              Http.ok200
              ( [("Content-Type", "text/html")]
                  <> if uniqueRunId /= qry.hasItBeenRestarted
                    then -- cause the client side to refresh
                      [("HX-Refresh", "true")]
                    else []
              )
              ""
        otherRoute -> h [fmt|/{otherRoute}|] (mainHtml uniqueRunId)
  where
    everySecond :: Text -> Enc -> Html -> Html
    everySecond call extraData innerHtml = [hsx|<div hx-trigger="every 1s" hx-swap="outerHTML" hx-post={call} hx-vals={Enc.encToBytesUtf8 extraData}>{innerHtml}</div>|]

    mainHtml uniqueRunId _span = runTransaction $ do
      jsonld <-
        httpGetJsonLd
          ( URI.parseURI "https://musicbrainz.org/work/92000fd4-d304-406d-aeb4-6bdbeed318ec" & annotate "not an URI" & unwrapError,
            "https://musicbrainz.org/work/92000fd4-d304-406d-aeb4-6bdbeed318ec"
          )
          <&> renderJsonld
      bestTorrentsTable <- getBestTorrentsTable
      -- transmissionTorrentsTable <- lift @Transaction getTransmissionTorrentsTable
      pure $
        Html.docTypeHtml
          [hsx|
      <head>
        <title>whatcd-resolver</title>
        <meta charset="utf-8">
        <meta name="viewport" content="width=device-width, initial-scale=1">
        <link href="https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/css/bootstrap.min.css" rel="stylesheet" integrity="sha384-9ndCyUaIbzAi2FUVXJi0CjmCapSmO7SnpJef0486qhLnuZ2cdeRhO02iuK6FUUVM" crossorigin="anonymous">
        <script src="https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/js/bootstrap.bundle.min.js" integrity="sha384-geWF76RCwLtnZ8qwWowPQNguL3RmwHVBC9FhGdlKrxdiJJigb/j/68SIy3Te4Bkz" crossorigin="anonymous"></script>
        <script src="https://unpkg.com/htmx.org@1.9.2" integrity="sha384-L6OqL9pRWyyFU3+/bjdSri+iIphTN/bvYyM37tICVyOJkWZLpP2vGn6VUEXgzg6h" crossorigin="anonymous"></script>
        <style>
          dl {
            margin: 1em;
            padding: 0.5em 1em;
            border: thin solid;
          }
        </style>
      </head>
      <body>
        {jsonld}
        <form
          hx-post="/snips/redacted/search"
          hx-target="#redacted-search-results">
          <label for="redacted-search">Redacted Search</label>
          <input
            id="redacted-search"
            type="text"
            name="redacted-search" />
          <button type="submit" hx-disabled-elt="this">Search</button>
          <div class="htmx-indicator">Search running!</div>
        </form>
        <div id="redacted-search-results">
          {bestTorrentsTable}
        </div>
        <!-- refresh the page if the uniqueRunId is different -->
        <input
             hidden
             type="text"
             id="autorefresh"
             name="hasItBeenRestarted"
             value={uniqueRunId}
             hx-get="/autorefresh"
             hx-trigger="every 5s"
             hx-swap="none"
        />
      </body>
    |]

singleQueryArgument :: Text -> FieldParser ByteString to -> Parse Http.Query to
singleQueryArgument field inner =
  Parse.mkParsePushContext
    field
    ( \(ctx, qry) -> case qry
        & mapMaybe
          ( \(k, v) ->
              if k == (field & textToBytesUtf8)
                then Just v
                else Nothing
          ) of
        [] -> Left [fmt|No such query argument "{field}", at {ctx & Parse.showContext}|]
        [Nothing] -> Left [fmt|Expected one query argument with a value, but "{field}" was a query flag|]
        [Just one] -> Right one
        more -> Left [fmt|More than one value for query argument "{field}": {show more}, at {ctx & Parse.showContext}|]
    )
    >>> Parse.fieldParser inner

-- | Make sure we can parse the given Text into an URI.
textToURI :: Parse Text URI
textToURI =
  Parse.fieldParser
    ( FieldParser $ \text ->
        text
          & textToString
          & Network.URI.parseURI
          & annotate [fmt|Cannot parse this as a URL: "{text}"|]
    )

-- | Make sure we can parse the given URI into a Request.
--
-- This tries to work around the horrible, horrible interface in Http.Client.
uriToHttpClientRequest :: Parse URI Http.Request
uriToHttpClientRequest =
  Parse.mkParseNoContext
    ( \url ->
        (url & Http.requestFromURI)
          & runCatch
          & first (checkException @Http.HttpException)
          & \case
            Left (Right (Http.InvalidUrlException urlText reason)) ->
              Left [fmt|Unable to set the url "{urlText}" as request URL, reason: {reason}|]
            Left (Right exc@(Http.HttpExceptionRequest _ _)) ->
              Left [fmt|Weird! Should not get a HttpExceptionRequest when parsing an URL (bad library design), was {exc & displayException}|]
            Left (Left someExc) ->
              Left [fmt|Weird! Should not get anyhting but a HttpException when parsing an URL (bad library design), was {someExc & displayException}|]
            Right req -> pure req
    )

checkException :: (Exception b) => SomeException -> Either SomeException b
checkException some = case fromException some of
  Nothing -> Left some
  Just e -> Right e

snipsRedactedSearch ::
  ( MonadLogger m,
    MonadPostgres m,
    HasField "searchstr" r ByteString,
    MonadThrow m,
    MonadTransmission m,
    MonadOtel m
  ) =>
  r ->
  m Html
snipsRedactedSearch dat = do
  t <-
    redactedSearchAndInsert
      [ ("searchstr", dat.searchstr),
        ("releasetype", "album")
      ]
  runTransaction $ do
    t
    getBestTorrentsTable

getBestTorrentsTable ::
  ( MonadTransmission m,
    MonadThrow m,
    MonadLogger m,
    MonadPostgres m,
    MonadOtel m
  ) =>
  Transaction m Html
getBestTorrentsTable = do
  bestStale :: [TorrentData ()] <- getBestTorrents
  actual <-
    getAndUpdateTransmissionTorrentsStatus
      ( bestStale
          & mapMaybe
            ( \td -> case td.torrentStatus of
                InTransmission h -> Just h
                _ -> Nothing
            )
          <&> (\t -> (getLabel @"torrentHash" t, t.transmissionInfo))
          & Map.fromList
      )
  let fresh =
        bestStale
          --  we have to update the status of every torrent that’s not in tranmission anymore
          -- TODO I feel like it’s easier (& more correct?) to just do the database request again …
          <&> ( \td -> case td.torrentStatus of
                  InTransmission info ->
                    case actual & Map.lookup (getLabel @"torrentHash" info) of
                      -- TODO this is also pretty dumb, cause it assumes that we have the torrent file if it was in transmission before,
                      -- which is an internal factum that is established in getBestTorrents (and might change later)
                      Nothing -> td {torrentStatus = NotInTransmissionYet}
                      Just transmissionInfo -> td {torrentStatus = InTransmission (T2 (getLabel @"torrentHash" info) (label @"transmissionInfo" transmissionInfo))}
                  NotInTransmissionYet -> td {torrentStatus = NotInTransmissionYet}
                  NoTorrentFileYet -> td {torrentStatus = NoTorrentFileYet}
              )
  let localTorrent b = case b.torrentStatus of
        NoTorrentFileYet -> [hsx|<button hx-post="snips/redacted/getTorrentFile" hx-swap="outerHTML" hx-vals={Enc.encToBytesUtf8 $ Enc.object [("torrent-id", Enc.int b.torrentId)]}>Upload Torrent</button>|]
        InTransmission info -> [hsx|{info.transmissionInfo.percentDone.unPercentage}% done|]
        NotInTransmissionYet -> [hsx|<button hx-post="snips/redacted/startTorrentFile" hx-swap="outerHTML" hx-vals={Enc.encToBytesUtf8 $ Enc.object [("torrent-id", Enc.int b.torrentId)]}>Start Torrent</button>|]
  let bestRows =
        fresh
          & foldMap
            ( \b -> do
                [hsx|
                  <tr>
                  <td>{localTorrent b}</td>
                  <td>{Html.toHtml @Int b.groupId}</td>
                  <td>{Html.toHtml @Text b.torrentGroupJson.artist}</td>
                  <td>{Html.toHtml @Text b.torrentGroupJson.groupName}</td>
                  <td>{Html.toHtml @Int b.seedingWeight}</td>
                  <td><details hx-trigger="toggle once" hx-post="snips/redacted/torrentDataJson" hx-vals={Enc.encToBytesUtf8 $ Enc.object [("torrent-id", Enc.int b.torrentId)]}></details></td>
                  </tr>
                |]
            )
  pure $
    [hsx|
        <table class="table">
          <thead>
            <tr>
              <th>Local</th>
              <th>Group ID</th>
              <th>Artist</th>
              <th>Name</th>
              <th>Weight</th>
              <th>Torrent</th>
              <th>Torrent Group</th>
            </tr>
          </thead>
          <tbody>
            {bestRows}
          </tbody>
        </table>
      |]

getTransmissionTorrentsTable ::
  (MonadTransmission m, MonadThrow m, MonadLogger m, MonadOtel m) => m Html
getTransmissionTorrentsTable = do
  let fields =
        [ "hashString",
          "name",
          "percentDone",
          "percentComplete",
          "downloadDir",
          "files"
        ]
  doTransmissionRequest'
    ( transmissionRequestListAllTorrents fields $ do
        Json.asObject <&> KeyMap.toMapText
    )
    <&> \resp ->
      Html.toTable
        ( resp
            & List.sortOn (\m -> m & Map.lookup "percentDone" & fromMaybe (Json.Number 0))
            <&> Map.toList
            -- TODO
            & List.take 100
        )

unzip3PGArray :: [(a1, a2, a3)] -> (PGArray a1, PGArray a2, PGArray a3)
unzip3PGArray xs = xs & unzip3 & \(a, b, c) -> (PGArray a, PGArray b, PGArray c)

assertOneUpdated ::
  (HasField "numberOfRowsAffected" r Natural, MonadThrow m, MonadIO m) =>
  Otel.Span ->
  Text ->
  r ->
  m ()
assertOneUpdated span name x = case x.numberOfRowsAffected of
  1 -> pure ()
  n -> appThrowTree span ([fmt|{name :: Text}: Expected to update exactly one row, but updated {n :: Natural} row(s)|])

migrate ::
  ( MonadPostgres m,
    MonadOtel m
  ) =>
  Transaction m (Label "numberOfRowsAffected" Natural)
migrate = inSpan "Database Migration" $ do
  execute
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

    ALTER TABLE redacted.torrents_json
    ADD COLUMN IF NOT EXISTS torrent_file bytea NULL;
    ALTER TABLE redacted.torrents_json
    ADD COLUMN IF NOT EXISTS transmission_torrent_hash text NULL;

    -- inflect out values of the full json

    CREATE OR REPLACE VIEW redacted.torrents AS
    SELECT
      t.id,
      t.torrent_id,
      t.torrent_group,
      -- the seeding weight is used to find the best torrent in a group.
      ( ((full_json_result->'seeders')::integer*3
        + (full_json_result->'snatches')::integer
        )
      -- prefer remasters by multiplying them with 3
      * (CASE
          WHEN full_json_result->>'remasterTitle' ILIKE '%remaster%'
          THEN 3
          ELSE 1
         END)
      )
      AS seeding_weight,
      t.full_json_result,
      t.torrent_file,
      t.transmission_torrent_hash
    FROM redacted.torrents_json t;

    CREATE INDEX IF NOT EXISTS torrents_json_seeding ON redacted.torrents_json(((full_json_result->'seeding')::integer));
    CREATE INDEX IF NOT EXISTS torrents_json_snatches ON redacted.torrents_json(((full_json_result->'snatches')::integer));
  |]
    ()

httpTorrent ::
  ( MonadIO m,
    MonadThrow m
  ) =>
  Otel.Span ->
  Http.Request ->
  m ByteString
httpTorrent span req =
  Http.httpBS req
    >>= assertM
      span
      ( \resp -> do
          let statusCode = resp & Http.responseStatus & (.statusCode)
              contentType =
                resp
                  & Http.responseHeaders
                  & List.lookup "content-type"
                  <&> Wai.parseContentType
                  <&> (\(ct, _mimeAttributes) -> ct)
          if
            | statusCode == 200,
              Just "application/x-bittorrent" <- contentType ->
                Right $ (resp & Http.responseBody)
            | statusCode == 200,
              Just otherType <- contentType ->
                Left [fmt|Redacted returned a non-torrent body, with content-type "{otherType}"|]
            | statusCode == 200,
              Nothing <- contentType ->
                Left [fmt|Redacted returned a body with unspecified content type|]
            | code <- statusCode -> Left [fmt|Redacted returned an non-200 error code, code {code}: {resp & showPretty}|]
      )

runAppWith :: AppT IO a -> IO (Either TmpPg.StartError a)
runAppWith appT = withTracer $ \tracer -> withDb $ \db -> do
  pgFormat <- readTools (label @"toolsEnvVar" "WHATCD_RESOLVER_TOOLS") (readTool "pg_format")
  let config = label @"logDatabaseQueries" LogDatabaseQueries
  pgConnPool <-
    Pool.newPool $
      Pool.defaultPoolConfig
        {- resource init action -} (Postgres.connectPostgreSQL (db & TmpPg.toConnectionString))
        {- resource destruction -} Postgres.close
        {- unusedResourceOpenTime -} 10
        {- max resources across all stripes -} 20
  transmissionSessionId <- newEmptyMVar
  let newAppT = do
        logInfo [fmt|Running with config: {showPretty config}|]
        logInfo [fmt|Connected to database at {db & TmpPg.toDataDirectory} on socket {db & TmpPg.toConnectionString}|]
        appT
  runReaderT newAppT.unAppT Context {..}

withTracer :: (Otel.Tracer -> IO c) -> IO c
withTracer f = do
  setDefaultEnv "OTEL_SERVICE_NAME" "whatcd-resolver"
  bracket
    -- Install the SDK, pulling configuration from the environment
    Otel.initializeGlobalTracerProvider
    -- Ensure that any spans that haven't been exported yet are flushed
    Otel.shutdownTracerProvider
    -- Get a tracer so you can create spans
    (\tracerProvider -> f $ Otel.makeTracer tracerProvider "whatcd-resolver" Otel.tracerOptions)

setDefaultEnv :: String -> String -> IO ()
setDefaultEnv envName defaultValue = do
  Env.lookupEnv envName >>= \case
    Just _env -> pure ()
    Nothing -> Env.setEnv envName defaultValue

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
            TmpPg.port = pure $ Just 5431,
            TmpPg.initDbConfig
          }
  TmpPg.withConfig cfg $ \db -> do
    -- print [fmt|data dir: {db & TmpPg.toDataDirectory}|]
    -- print [fmt|conn string: {db & TmpPg.toConnectionString}|]
    act db
