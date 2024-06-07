{-# LANGUAGE QuasiQuotes #-}

module WhatcdResolver where

import AppT
import Arg
import Control.Category qualified as Cat
import Control.Monad.Catch.Pure (runCatch)
import Control.Monad.Logger.CallStack
import Control.Monad.Reader
import Data.Aeson qualified as Json
import Data.Aeson.BetterErrors qualified as Json
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Error.Tree (prettyErrorTree)
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
import IHP.HSX.ToHtml (ToHtml)
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
import Network.Wai (ResponseReceived)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Parse qualified as Wai
import OpenTelemetry.Attributes qualified as Otel
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
import Text.Blaze.Html.Renderer.Utf8 qualified as Html
import Text.Blaze.Html5 qualified as Html
import Tool (readTool, readTools)
import Transmission
import UnliftIO hiding (Handler)
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
  uniqueRunId <-
    runTransaction $
      querySingleRowWith
        [sql|
            SELECT gen_random_uuid()::text
        |]
        ()
        (Dec.fromField @Text)

  withRunInIO $ \runInIO -> Warp.run 9093 $ \req respondOrig -> do
    let catchAppException act =
          try act >>= \case
            Right a -> pure a
            Left (AppException err) -> do
              runInIO (logError err)
              respondOrig (Wai.responseLBS Http.status500 [] "")

    catchAppException $ do
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

      let parseQueryArgsNewSpan spanName parser =
            Parse.runParse "Unable to find the right request query arguments" (lmap Wai.queryString parser) req
              & assertMNewSpan spanName id

      let handlers :: Handlers (AppT IO)
          handlers respond =
            Map.fromList
              [ ("", respond.html (mainHtml uniqueRunId)),
                ( "snips/redacted/search",
                  respond.html $
                    \span -> do
                      dat <-
                        mp
                          span
                          ( do
                              label @"searchstr" <$> Multipart.field "redacted-search" Cat.id
                          )
                      snipsRedactedSearch dat
                ),
                ( "snips/redacted/torrentDataJson",
                  respond.html $ \span -> do
                    dat <- torrentIdMp span
                    Html.mkVal <$> (runTransaction $ getTorrentById dat)
                ),
                ( "snips/redacted/getTorrentFile",
                  respond.html $ \span -> do
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
                ),
                -- TODO: this is bad duplication??
                ( "snips/redacted/startTorrentFile",
                  respond.html $ \span -> do
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
                ),
                ( "snips/transmission/getTorrentState",
                  respond.html $ \span -> do
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
                ),
                ( "snips/jsonld/render",
                  do
                    let HandlerResponses {htmlWithQueryArgs} = respond
                    htmlWithQueryArgs
                      ( label @"target"
                          <$> ( (singleQueryArgument "target" Field.utf8 >>> textToURI)
                                  & Parse.andParse uriToHttpClientRequest
                              )
                      )
                      ( \qry _span -> do
                          jsonld <- httpGetJsonLd (qry.target)
                          pure $ renderJsonld jsonld
                      )
                ),
                ( "artist",
                  do
                    let HandlerResponses {htmlWithQueryArgs} = respond

                    htmlWithQueryArgs
                      ( label @"artistRedactedId"
                          <$> (singleQueryArgument "redacted_id" (Field.utf8 >>> Field.decimalNatural))
                      )
                      $ \qry _span -> do
                        artistPage qry
                ),
                ( "autorefresh",
                  respond.plain $ do
                    qry <-
                      parseQueryArgsNewSpan
                        "Autorefresh Query Parse"
                        ( label @"hasItBeenRestarted"
                            <$> singleQueryArgument "hasItBeenRestarted" Field.utf8
                        )
                    pure $
                      Wai.responseLBS
                        Http.ok200
                        ( [("Content-Type", "text/html")]
                            <> if uniqueRunId /= qry.hasItBeenRestarted
                              then -- cause the client side to refresh
                                [("HX-Refresh", "true")]
                              else []
                        )
                        ""
                )
              ]
      runInIO $
        runHandlers
          (\respond -> respond.html $ (mainHtml uniqueRunId))
          handlers
          req
          respondOrig
  where
    everySecond :: Text -> Enc -> Html -> Html
    everySecond call extraData innerHtml = [hsx|<div hx-trigger="every 1s" hx-swap="outerHTML" hx-post={call} hx-vals={Enc.encToBytesUtf8 extraData}>{innerHtml}</div>|]

    mainHtml :: Text -> Otel.Span -> AppT IO Html
    mainHtml uniqueRunId _span = runTransaction $ do
      -- jsonld <-
      --   httpGetJsonLd
      --     ( URI.parseURI "https://musicbrainz.org/work/92000fd4-d304-406d-aeb4-6bdbeed318ec" & annotate "not an URI" & unwrapError,
      --       "https://musicbrainz.org/work/92000fd4-d304-406d-aeb4-6bdbeed318ec"
      --     )
      --     <&> renderJsonld
      bestTorrentsTable <- getBestTorrentsTable Nothing
      -- transmissionTorrentsTable <- lift @Transaction getTransmissionTorrentsTable
      pure $
        htmlPageChrome
          "whatcd-resolver"
          [hsx|
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
        |]

htmlPageChrome :: (ToHtml a) => Text -> a -> Html
htmlPageChrome title body =
  Html.docTypeHtml $
    [hsx|
      <head>
        <!-- TODO: set nice page title for each page -->
        <title>{title}</title>
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
        {body}
      </body>
    |]

artistPage ::
  ( HasField "artistRedactedId" dat Natural,
    MonadPostgres m,
    MonadOtel m,
    MonadLogger m,
    MonadThrow m,
    MonadTransmission m
  ) =>
  dat ->
  m Html
artistPage dat = runTransaction $ do
  fresh <- getBestTorrentsData (Just $ getLabel @"artistRedactedId" dat)
  let artistName = fresh & findMaybe (\t -> t.artists & findMaybe (\a -> if a.artistId == (dat.artistRedactedId & fromIntegral @Natural @Int) then Just a.artistName else Nothing))
  let torrents = mkBestTorrentsTable fresh
  pure $
    htmlPageChrome
      ( case artistName of
          Nothing -> "whatcd-resolver"
          Just a -> [fmt|{a} - Artist Page - whatcd-resolver|]
      )
      [hsx|
        Artist ID: {dat.artistRedactedId}

        {torrents}
      |]

type Handlers m = HandlerResponses m -> Map Text (m ResponseReceived)

data HandlerResponses m = HandlerResponses
  { -- | render html
    html :: (Otel.Span -> m Html) -> m ResponseReceived,
    -- | render html after parsing some query arguments
    htmlWithQueryArgs :: forall a. (Parse Query a -> (a -> Otel.Span -> m Html) -> m ResponseReceived),
    -- | render a plain wai response
    plain :: (m Wai.Response -> m ResponseReceived)
  }

runHandlers ::
  (MonadOtel m) =>
  (HandlerResponses m -> m ResponseReceived) ->
  (HandlerResponses m -> Map Text (m ResponseReceived)) ->
  Wai.Request ->
  (Wai.Response -> IO ResponseReceived) ->
  m ResponseReceived
runHandlers defaultHandler handlers req respond = withRunInIO $ \runInIO -> do
  let path = req & Wai.pathInfo & Text.intercalate "/"
  let html act =
        Otel.inSpan'
          [fmt|Route /{path}|]
          ( Otel.defaultSpanArguments
              { Otel.attributes =
                  HashMap.fromList
                    [ ("_.server.path", Otel.toAttribute @Text path),
                      ("_.server.query_args", Otel.toAttribute @Text (req.rawQueryString & bytesToTextUtf8Lenient))
                    ]
              }
          )
          ( \span -> do
              res <- act span <&> (\h -> T2 (label @"html" h) (label @"extraHeaders" []))
              liftIO $ respond . Wai.responseLBS Http.ok200 ([("Content-Type", "text/html")] <> res.extraHeaders) . Html.renderHtml $ res.html
          )

  let handlerResponses =
        ( HandlerResponses
            { plain = (\m -> liftIO $ runInIO m >>= respond),
              html,
              htmlWithQueryArgs = \parser act ->
                case req & Parse.runParse "Unable to find the right request query arguments" (lmap Wai.queryString parser) of
                  Right a -> html (act a)
                  Left err ->
                    html
                      ( \span -> do
                          recordException
                            span
                            ( T2
                                (label @"type_" "Query Parse Exception")
                                (label @"message" (prettyErrorTree err))
                            )

                          pure
                            [hsx|
                              <h1>Error:</h1>
                              <pre>{err & prettyErrorTree}</pre>
                            |]
                      )
            }
        )
  let handler =
        (handlers handlerResponses)
          & Map.lookup path
          & fromMaybe (defaultHandler handlerResponses)
  runInIO handler

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
    getBestTorrentsTable (Nothing :: Maybe (Label "artistRedactedId" Natural))

data ArtistFilter = ArtistFilter
  { onlyArtist :: Maybe (Label "artistId" Text)
  }

getBestTorrentsTable ::
  ( MonadTransmission m,
    MonadThrow m,
    MonadLogger m,
    MonadPostgres m,
    MonadOtel m
  ) =>
  Maybe (Label "artistRedactedId" Natural) ->
  Transaction m Html
getBestTorrentsTable dat = do
  fresh <- getBestTorrentsData dat
  pure $ mkBestTorrentsTable fresh

getBestTorrentsData ::
  ( MonadTransmission m,
    MonadThrow m,
    MonadLogger m,
    MonadPostgres m,
    MonadOtel m
  ) =>
  Maybe (Label "artistRedactedId" Natural) ->
  Transaction m [TorrentData (Label "percentDone" Percentage)]
getBestTorrentsData artistFilter = do
  bestStale :: [TorrentData ()] <- getBestTorrents GetBestTorrentsFilter {onlyArtist = artistFilter, onlyDownloaded = False}
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
  pure $
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

mkBestTorrentsTable :: [TorrentData (Label "percentDone" Percentage)] -> Html
mkBestTorrentsTable fresh = do
  let localTorrent b = case b.torrentStatus of
        NoTorrentFileYet -> [hsx|<button hx-post="snips/redacted/getTorrentFile" hx-swap="outerHTML" hx-vals={Enc.encToBytesUtf8 $ Enc.object [("torrent-id", Enc.int b.torrentId)]}>Upload Torrent</button>|]
        InTransmission info -> [hsx|{info.transmissionInfo.percentDone.unPercentage}% done|]
        NotInTransmissionYet -> [hsx|<button hx-post="snips/redacted/startTorrentFile" hx-swap="outerHTML" hx-vals={Enc.encToBytesUtf8 $ Enc.object [("torrent-id", Enc.int b.torrentId)]}>Start Torrent</button>|]
  let bestRows =
        fresh
          & foldMap
            ( \b -> do
                let artists =
                      b.artists
                        <&> ( \a ->
                                T2
                                  (label @"url" [fmt|/artist?redacted_id={a.artistId}|])
                                  (label @"content" $ Html.toHtml @Text a.artistName)
                            )
                        & mkLinkList

                [hsx|
                  <tr>
                  <td>{localTorrent b}</td>
                  <td>{Html.toHtml @Int b.groupId}</td>
                  <td>
                    {artists}
                  </td>
                  <td>
                    <a href={mkRedactedTorrentLink (Arg b.groupId)} target="_blank">
                      {Html.toHtml @Text b.torrentGroupJson.groupName}
                    </a>
                  </td>
                  <td>{Html.toHtml @Int b.torrentGroupJson.groupYear}</td>
                  <td>{Html.toHtml @Int b.seedingWeight}</td>
                  <td><details hx-trigger="toggle once" hx-post="snips/redacted/torrentDataJson" hx-vals={Enc.encToBytesUtf8 $ Enc.object [("torrent-id", Enc.int b.torrentId)]}></details></td>
                  </tr>
                |]
            )
  [hsx|
        <table class="table">
          <thead>
            <tr>
              <th>Local</th>
              <th>Group ID</th>
              <th>Artist</th>
              <th>Name</th>
              <th>Year</th>
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

mkLinkList :: [T2 "url" Text "content" Html] -> Html
mkLinkList xs =
  xs
    <&> ( \x -> do
            [hsx|<a href={x.url}>{x.content}</a>|]
        )
    & List.intersperse ", "
    & mconcat

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

    CREATE INDEX IF NOT EXISTS redacted_torrents_json_torrent_group_fk ON redacted.torrents_json (torrent_group);


    ALTER TABLE redacted.torrents_json
    ADD COLUMN IF NOT EXISTS torrent_file bytea NULL;
    ALTER TABLE redacted.torrents_json
    ADD COLUMN IF NOT EXISTS transmission_torrent_hash text NULL;


    -- the seeding weight is used to find the best torrent in a group.
    CREATE OR REPLACE FUNCTION calc_seeding_weight(full_json_result jsonb) RETURNS int AS $$
    BEGIN
      RETURN
        ((full_json_result->'seeders')::integer*3
        + (full_json_result->'snatches')::integer
        )
        -- prefer remasters by multiplying them with 3
        * (CASE
            WHEN full_json_result->>'remasterTitle' ILIKE '%remaster%'
            THEN 3
            ELSE 1
          END);
    END;
    $$ LANGUAGE plpgsql IMMUTABLE;

    ALTER TABLE redacted.torrents_json
    ADD COLUMN IF NOT EXISTS seeding_weight int GENERATED ALWAYS AS (calc_seeding_weight(full_json_result)) STORED;

    -- inflect out values of the full json
    CREATE OR REPLACE VIEW redacted.torrents AS
    SELECT
      t.id,
      t.torrent_id,
      t.torrent_group,
      -- the seeding weight is used to find the best torrent in a group.
      t.seeding_weight,
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
  tool <- readTools (label @"toolsEnvVar" "WHATCD_RESOLVER_TOOLS") (readTool "pg_format")
  pgFormat <- initPgFormatPool (label @"pgFormat" tool)
  let config = label @"logDatabaseQueries" LogDatabaseQueries
  pgConnPool <-
    Pool.newPool $
      Pool.defaultPoolConfig
        {- resource init action -} (Postgres.connectPostgreSQL (db & TmpPg.toConnectionString))
        {- resource destruction -} Postgres.close
        {- unusedResourceOpenTime -} 10
        {- max resources across all stripes -} 20
  transmissionSessionId <- newIORef Nothing
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
    ( do
        (processors, opts) <- Otel.getTracerProviderInitializationOptions
        tp <-
          Otel.createTracerProvider
            processors
            -- workaround the attribute length bug https://github.com/iand675/hs-opentelemetry/issues/113
            ( opts
                { Otel.tracerProviderOptionsAttributeLimits =
                    opts.tracerProviderOptionsAttributeLimits
                      { Otel.attributeCountLimit = Just 65_000
                      }
                }
            )
        Otel.setGlobalTracerProvider tp
        pure tp
    )
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
