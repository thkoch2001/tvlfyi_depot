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
import Data.ByteString.Builder qualified as Builder
import Data.Error.Tree
import Data.HashMap.Strict qualified as HashMap
import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Pool qualified as Pool
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Database.PostgreSQL.Simple (Binary (Binary), Only (..))
import Database.PostgreSQL.Simple qualified as Postgres
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.Types (PGArray (PGArray))
import Database.Postgres.Temp qualified as TmpPg
import FieldParser (FieldParser, FieldParser' (..))
import FieldParser qualified as Field
import GHC.Records (HasField (..))
import Html qualified
import IHP.HSX.QQ (hsx)
import Json qualified
import Json.Enc (Enc)
import Json.Enc qualified as Enc
import Label
import Multipart2 qualified as Multipart
import Network.HTTP.Client.Conduit qualified as Http
import Network.HTTP.Simple qualified as Http
import Network.HTTP.Types
import Network.HTTP.Types qualified as Http
import Network.HTTP.Types.URI qualified as Url
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
import PossehlAnalyticsPrelude
import Postgres.Decoder qualified as Dec
import Postgres.MonadPostgres
import Pretty
import RunCommand (runCommandExpect0)
import System.Directory qualified as Dir
import System.Directory qualified as Xdg
import System.Environment qualified as Env
import System.FilePath ((</>))
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Pretty qualified as Html.Pretty
import Text.Blaze.Html.Renderer.Utf8 qualified as Html
import Text.Blaze.Html5 qualified as Html
import Tool (readTool, readTools)
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
  withRunInIO $ \runInIO -> Warp.run 9092 $ \req respond -> do
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
      let h route act =
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
                    respond . Wai.responseLBS Http.ok200 [("Content-Type", "text/html")] . renderHtml $ res
                )

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

      case req & Wai.pathInfo & Text.intercalate "/" of
        "" -> h "/" mainHtml
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
        otherRoute -> h [fmt|/{otherRoute}|] mainHtml
  where
    everySecond :: Text -> Enc -> Html -> Html
    everySecond call extraData innerHtml = [hsx|<div hx-trigger="every 1s" hx-swap="outerHTML" hx-post={call} hx-vals={Enc.encToBytesUtf8 extraData}>{innerHtml}</div>|]

    mainHtml _span = runTransaction $ do
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

-- | A recursive `json+ld` structure.
data Jsonld
  = JsonldObject JsonldObject
  | JsonldArray [Jsonld]
  | JsonldField Json.Value
  deriving stock (Show, Eq)

-- | A json+ld object, that is something which can be further expanded by following the URL in its `id_` field.
data JsonldObject = JsonldObject'
  { -- | `@type` field; currently just the plain value without taking into account the json+ld context
    type_ :: Set Text,
    -- | `@id` field, usually a link to follow for expanding the object to its full glory
    id_ :: Text,
    -- | any fields of this object that remote deemed important enough to already pre-emptively include in the object; to get all fields resolve the URL in `id_`.
    previewFields :: Map Text Jsonld
  }
  deriving stock (Show, Eq)

jsonldParser :: (Monad m) => Json.ParseT err m Jsonld
jsonldParser =
  Json.asValue >>= \cur -> do
    if
      | Json.Object _ <- cur -> do
          typeMay <- Json.keyMay "@type" $ (Json.asArraySet Json.asText Json.<|> (Set.singleton <$> Json.asText))
          idMay <- Json.keyMay "@id" $ Json.asText
          if
            | Just type_ <- typeMay,
              Just id_ <- idMay -> do
                previewFields <-
                  Json.asObjectMap jsonldParser
                    <&> Map.delete "@type"
                    <&> Map.delete "@id"
                pure $ JsonldObject $ JsonldObject' {..}
            | otherwise -> pure $ JsonldField cur
      | Json.Array _ <- cur -> do
          JsonldArray <$> Json.eachInArray jsonldParser
      | otherwise -> pure $ JsonldField cur

renderJsonld :: Jsonld -> Html
renderJsonld = \case
  JsonldObject obj ->
    [hsx|
    <dl>
      <dt>Type</dt>
      <dd>{obj.type_ & toList & schemaTypes}</dd>
      <dt>Url</dt>
      <dd><a href={obj.id_}>{obj.id_}</a></dd>
      <dt>Fields</dt>
      <dd>
        {obj.previewFields & Html.toDefinitionList schemaType renderJsonld}
        <div>
          <button
            hx-get={snippetHref obj.id_}
            hx-target="closest dl"
            hx-swap="outerHTML"
          >more fields …</button>
        </div>
      </dd>
    </dl>
  |]
    where
      snippetHref target =
        Builder.toLazyByteString $
          "/snips/jsonld/render"
            <> Url.renderQueryBuilder True [("target", Just (textToBytesUtf8 target))]

      schemaTypes xs =
        xs
          <&> schemaType
          & List.intersperse ", "
          & mconcat
      schemaType t =
        let href :: Text = [fmt|https://schema.org/{t}|] in [hsx|<a href={href} target="_blank">{t}</a>|]
  JsonldArray arr ->
    Html.toOrderedList renderJsonld arr
  JsonldField f -> Html.mkVal f

-- | A value between (inclusive) 0% and (inclusive) 100%. Precise to 1% steps.
newtype Percentage = Percentage {unPercentage :: Int}
  deriving stock (Show)

-- | Parse a scientific into a Percentage
scientificPercentage :: FieldParser' Error Scientific Percentage
scientificPercentage =
  Field.boundedScientificRealFloat @Float
    >>> ( FieldParser $ \f ->
            if
              | f < 0 -> Left "percentage cannot be negative"
              | f > 1 -> Left "percentage cannot be over 100%"
              | otherwise -> Right $ Percentage $ ceiling (f * 100)
        )

-- | Fetch the current status from transmission, and remove the tranmission hash from our database
-- iff it does not exist in transmission anymore
getAndUpdateTransmissionTorrentsStatus ::
  ( MonadTransmission m,
    MonadThrow m,
    MonadLogger m,
    MonadPostgres m,
    MonadOtel m
  ) =>
  Map (Label "torrentHash" Text) () ->
  (Transaction m (Map (Label "torrentHash" Text) (Label "percentDone" Percentage)))
getAndUpdateTransmissionTorrentsStatus knownTorrents = do
  let fields = ["hashString", "percentDone"]
  actualTorrents <-
    lift @Transaction $
      doTransmissionRequest'
        ( transmissionRequestListOnlyTorrents
            ( T2
                (label @"fields" fields)
                (label @"ids" (Map.keys knownTorrents))
            )
            $ do
              torrentHash <- Json.keyLabel @"torrentHash" "hashString" Json.asText
              percentDone <- Json.keyLabel @"percentDone" "percentDone" (Field.toJsonParser $ Field.jsonNumber >>> scientificPercentage)
              pure (torrentHash, percentDone)
        )
        <&> Map.fromList
  let toDelete = Map.difference knownTorrents actualTorrents
  execute
    [fmt|
    UPDATE redacted.torrents_json
    SET transmission_torrent_hash = NULL
    WHERE transmission_torrent_hash = ANY (?::text[])
  |]
    $ Only (toDelete & Map.keys <&> (.torrentHash) & PGArray :: PGArray Text)
  pure actualTorrents

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

data TransmissionRequest = TransmissionRequest
  { method :: Text,
    arguments :: Map Text Enc,
    tag :: Maybe Int
  }
  deriving stock (Show)

testTransmission :: (Show out) => (TransmissionRequest, Json.Parse Error out) -> IO (Either TmpPg.StartError ())
testTransmission req = runAppWith $ inSpan' "Test Transmission" $ \span ->
  doTransmissionRequest
    span
    transmissionConnectionConfig
    req
    >>= liftIO . printPretty

transmissionConnectionConfig :: T2 "host" Text "port" Text
transmissionConnectionConfig = (T2 (label @"host" "localhost") (label @"port" "9091"))

transmissionRequestListAllTorrents :: (Monad m) => [Text] -> Json.ParseT e m out -> (TransmissionRequest, Json.ParseT e m [out])
transmissionRequestListAllTorrents fields parseTorrent =
  ( TransmissionRequest
      { method = "torrent-get",
        arguments =
          Map.fromList
            [ ("fields", Enc.list Enc.text fields)
            ],
        tag = Nothing
      },
    Json.key "torrents" $ Json.eachInArray parseTorrent
  )

transmissionRequestListOnlyTorrents ::
  ( HasField "ids" r1 [(Label "torrentHash" Text)],
    HasField "fields" r1 [Text],
    Monad m
  ) =>
  r1 ->
  Json.ParseT e m out ->
  (TransmissionRequest, Json.ParseT e m [out])
transmissionRequestListOnlyTorrents dat parseTorrent =
  ( TransmissionRequest
      { method = "torrent-get",
        arguments =
          Map.fromList
            [ ("ids", Enc.list (\i -> Enc.text i.torrentHash) dat.ids),
              ("fields", Enc.list Enc.text dat.fields)
            ],
        tag = Nothing
      },
    Json.key "torrents" $ Json.eachInArray parseTorrent
  )

transmissionRequestAddTorrent ::
  (HasField "torrentFile" r ByteString, Monad m) =>
  r ->
  ( TransmissionRequest,
    Json.ParseT err m (T2 "torrentHash" Text "torrentName" Text)
  )
transmissionRequestAddTorrent dat =
  ( TransmissionRequest
      { method = "torrent-add",
        arguments =
          Map.fromList
            [ ("metainfo", Enc.base64Bytes dat.torrentFile),
              ("paused", Enc.bool False)
            ],
        tag = Nothing
      },
    do
      let p method = Json.key method $ do
            hash <- Json.keyLabel @"torrentHash" "hashString" Json.asText
            name <- Json.keyLabel @"torrentName" "name" Json.asText
            pure $ T2 hash name
      p "torrent-duplicate" Json.<|> p "torrent-added"
  )

data TransmissionResponse output = TransmissionResponse
  { result :: TransmissionResponseStatus,
    arguments :: Maybe output,
    tag :: Maybe Int
  }
  deriving stock (Show)

data TransmissionResponseStatus
  = TransmissionResponseSuccess
  | TransmissionResponseFailure Text
  deriving stock (Show)

doTransmissionRequest' ::
  ( MonadTransmission m,
    MonadThrow m,
    MonadLogger m,
    MonadOtel m
  ) =>
  (TransmissionRequest, Json.Parse Error output) ->
  m output
doTransmissionRequest' req = inSpan' "Transmission Request" $ \span -> do
  resp <-
    doTransmissionRequest
      span
      transmissionConnectionConfig
      req
  case resp.result of
    TransmissionResponseFailure err -> appThrowTree span (nestedError "Transmission RPC error" $ singleError $ newError err)
    TransmissionResponseSuccess -> case resp.arguments of
      Nothing -> appThrowTree span "Transmission RPC error: No `arguments` field in response"
      Just out -> pure out

-- | Contact the transmission RPC, and do the CSRF protection dance.
--
-- Spec: https://github.com/transmission/transmission/blob/main/docs/rpc-spec.md
doTransmissionRequest ::
  ( MonadTransmission m,
    HasField "host" t1 Text,
    HasField "port" t1 Text,
    MonadThrow m,
    MonadLogger m,
    Otel.MonadTracer m,
    MonadUnliftIO m
  ) =>
  Otel.Span ->
  t1 ->
  (TransmissionRequest, Json.Parse Error output) ->
  m (TransmissionResponse output)
doTransmissionRequest span dat (req, parser) = do
  sessionId <- getTransmissionId
  let textArg t = (Enc.text t, Otel.toAttribute @Text t)
  let encArg enc = (enc, Otel.toAttribute @Text $ enc & Enc.encToTextPretty)
  let intArg i = (Enc.int i, Otel.toAttribute @Int i)

  let body :: [(Text, (Enc, Otel.Attribute))] =
        ( [ ("method", req.method & textArg),
            ("arguments", encArg $ Enc.map id req.arguments)
          ]
            <> (req.tag & foldMap (\t -> [("tag", t & intArg)]))
        )
  addAttributes
    span
    ( HashMap.fromList $
        body
          <&> bimap
            (\k -> [fmt|transmission.{k}|])
            (\(_, attr) -> attr)
    )
  let httpReq =
        [fmt|http://{dat.host}:{dat.port}/transmission/rpc|]
          & Http.setRequestMethod "POST"
          & Http.setRequestBodyLBS (Enc.encToBytesUtf8Lazy (body <&> second fst & Enc.object))
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
      doTransmissionRequest span dat (req, parser)
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
                Json.keyMay "arguments" parser
              tag <-
                Json.keyMay
                  "tag"
                  (Field.toJsonParser (Field.jsonNumber >>> Field.boundedScientificIntegral "tag too long"))
              pure TransmissionResponse {..}
          )
        & first (Json.parseErrorTree "Cannot parse transmission RPC response")
        & \case
          Right a -> pure a
          Left err -> do
            case Json.eitherDecodeStrict' @Json.Value (resp & Http.getResponseBody) of
              Left _err -> pure ()
              Right val -> logInfo [fmt|failing transmission response: {showPrettyJson val}|]
            appThrowTree span err
    _ -> liftIO $ unwrapIOError $ Left [fmt|Non-200 response: {showPretty resp}|]

redactedSearch ::
  (MonadLogger m, MonadThrow m, MonadOtel m) =>
  [(ByteString, ByteString)] ->
  Json.Parse ErrorTree a ->
  m a
redactedSearch advanced parser =
  inSpan "Redacted API Search" $
    redactedApiRequestJson
      ( T2
          (label @"action" "browse")
          (label @"actionArgs" ((advanced <&> second Just)))
      )
      parser

redactedGetTorrentFile ::
  ( MonadLogger m,
    MonadThrow m,
    HasField "torrentId" dat Int,
    MonadOtel m
  ) =>
  dat ->
  m ByteString
redactedGetTorrentFile dat = inSpan' "Redacted Get Torrent File" $ \span -> do
  req <-
    mkRedactedApiRequest
      ( T2
          (label @"action" "download")
          ( label @"actionArgs"
              [ ("id", Just (dat.torrentId & showToText @Int & textToBytesUtf8))
              -- try using tokens as long as we have them (TODO: what if there’s no tokens left?
              -- ANSWER: it breaks:
              -- responseBody = "{\"status\":\"failure\",\"error\":\"You do not have any freeleech tokens left. Please use the regular DL link.\"}",
              -- ("usetoken", Just "1")
              ]
          )
      )
  httpTorrent span req

-- fix
--   ( \io -> do
--       logInfo "delay"
--       liftIO $ threadDelay 10_000_000
--       io
--   )

exampleSearch :: (MonadThrow m, MonadLogger m, MonadPostgres m, MonadOtel m) => m (Transaction m ())
exampleSearch = do
  t1 <-
    redactedSearchAndInsert
      [ ("searchstr", "cherish"),
        ("artistname", "kirinji"),
        -- ("year", "1982"),
        -- ("format", "MP3"),
        -- ("releasetype", "album"),
        ("order_by", "year")
      ]
  t3 <-
    redactedSearchAndInsert
      [ ("searchstr", "mouss et hakim"),
        ("artistname", "mouss et hakim"),
        -- ("year", "1982"),
        -- ("format", "MP3"),
        -- ("releasetype", "album"),
        ("order_by", "year")
      ]
  t2 <-
    redactedSearchAndInsert
      [ ("searchstr", "thriller"),
        ("artistname", "michael jackson"),
        -- ("year", "1982"),
        -- ("format", "MP3"),
        -- ("releasetype", "album"),
        ("order_by", "year")
      ]
  pure (t1 >> t2 >> t3)

-- | Do the search, return a transaction that inserts all results from all pages of the search.
redactedSearchAndInsert ::
  forall m.
  ( MonadLogger m,
    MonadPostgres m,
    MonadThrow m,
    MonadOtel m
  ) =>
  [(ByteString, ByteString)] ->
  m (Transaction m ())
redactedSearchAndInsert extraArguments = do
  logInfo [fmt|Doing redacted search with arguments: {showPretty extraArguments}|]
  -- The first search returns the amount of pages, so we use that to query all results piece by piece.
  firstPage <- go Nothing
  let remainingPages = firstPage.pages - 1
  logInfo [fmt|Got the first page, found {remainingPages} more pages|]
  let otherPagesNum = [(2 :: Natural) .. remainingPages]
  otherPages <- traverse go (Just <$> otherPagesNum)
  pure $
    (firstPage : otherPages)
      & concatMap (.tourGroups)
      & insertTourGroupsAndTorrents
  where
    go mpage =
      redactedSearch
        ( extraArguments
            -- pass the page (for every search but the first one)
            <> ifExists (mpage <&> (\page -> [("page", (page :: Natural) & showToText & textToBytesUtf8)]))
        )
        ( do
            status <- Json.key "status" Json.asText
            when (status /= "success") $ do
              Json.throwCustomError [fmt|Status was not "success", but {status}|]
            Json.key "response" $ do
              pages <-
                Json.keyMay "pages" (Field.toJsonParser (Field.mapError singleError $ Field.jsonNumber >>> Field.boundedScientificIntegral @Int "not an Integer" >>> Field.integralToNatural))
                  -- in case the field is missing, let’s assume there is only one page
                  <&> fromMaybe 1
              Json.key "results" $ do
                tourGroups <-
                  label @"tourGroups"
                    <$> ( Json.eachInArray $ do
                            groupId <- Json.keyLabel @"groupId" "groupId" (Json.asIntegral @_ @Int)
                            groupName <- Json.keyLabel @"groupName" "groupName" Json.asText
                            fullJsonResult <-
                              label @"fullJsonResult"
                                <$> ( Json.asObject
                                        -- remove torrents cause they are inserted separately below
                                        <&> KeyMap.filterWithKey (\k _ -> k /= "torrents")
                                        <&> Json.Object
                                    )
                            let tourGroup = T3 groupId groupName fullJsonResult
                            torrents <- Json.keyLabel @"torrents" "torrents" $
                              Json.eachInArray $ do
                                torrentId <- Json.keyLabel @"torrentId" "torrentId" (Json.asIntegral @_ @Int)
                                fullJsonResultT <- label @"fullJsonResult" <$> Json.asValue
                                pure $ T2 torrentId fullJsonResultT
                            pure (T2 (label @"tourGroup" tourGroup) torrents)
                        )
                pure
                  ( T2
                      (label @"pages" pages)
                      tourGroups
                  )
        )
    insertTourGroupsAndTorrents ::
      [ T2
          "tourGroup"
          (T3 "groupId" Int "groupName" Text "fullJsonResult" Json.Value)
          "torrents"
          [T2 "torrentId" Int "fullJsonResult" Json.Value]
      ] ->
      Transaction m ()
    insertTourGroupsAndTorrents dat = do
      let tourGroups = dat <&> (.tourGroup)
      let torrents = dat <&> (.torrents)
      insertTourGroups tourGroups
        >>= ( \res ->
                insertTorrents $
                  zipT2 $
                    T2
                      (label @"torrentGroupIdPg" $ res <&> (.tourGroupIdPg))
                      (label @"torrents" torrents)
            )
    insertTourGroups ::
      [ T3
          "groupId"
          Int
          "groupName"
          Text
          "fullJsonResult"
          Json.Value
      ] ->
      Transaction m [Label "tourGroupIdPg" Int]
    insertTourGroups dats = do
      let groupNames =
            [ [fmt|{dat.groupId}: {dat.groupName}|]
              | dat <- dats
            ]
      logInfo [fmt|Inserting tour groups for {showPretty groupNames}|]
      _ <-
        execute
          [fmt|
                  DELETE FROM redacted.torrent_groups
                  WHERE group_id = ANY (?::integer[])
              |]
          (Only $ (dats <&> (.groupId) & PGArray :: PGArray Int))
      executeManyReturningWith
        [fmt|
              INSERT INTO redacted.torrent_groups (
                group_id, group_name, full_json_result
              ) VALUES
              ( ?, ? , ? )
              ON CONFLICT (group_id) DO UPDATE SET
                group_id = excluded.group_id,
                group_name = excluded.group_name,
                full_json_result = excluded.full_json_result
              RETURNING (id)
            |]
        ( dats <&> \dat ->
            ( dat.groupId,
              dat.groupName,
              dat.fullJsonResult
            )
        )
        (label @"tourGroupIdPg" <$> Dec.fromField @Int)

    insertTorrents ::
      [ T2
          "torrentGroupIdPg"
          Int
          "torrents"
          [T2 "torrentId" Int "fullJsonResult" Json.Value]
      ] ->
      Transaction m ()
    insertTorrents dats = do
      _ <-
        execute
          [sql|
            DELETE FROM redacted.torrents_json
            WHERE torrent_id = ANY (?::integer[])
          |]
          ( Only $
              PGArray
                [ torrent.torrentId
                  | dat <- dats,
                    torrent <- dat.torrents
                ]
          )

      execute
        [sql|
          INSERT INTO redacted.torrents_json
            ( torrent_group
            , torrent_id
            , full_json_result)
          SELECT *
          FROM UNNEST(
              ?::integer[]
            , ?::integer[]
            , ?::jsonb[]
          ) AS inputs(
              torrent_group
            , torrent_id
            , full_json_result)
          |]
        ( [ ( dat.torrentGroupIdPg :: Int,
              group.torrentId :: Int,
              group.fullJsonResult :: Json.Value
            )
            | dat <- dats,
              group <- dat.torrents
          ]
            & unzip3PGArray
        )
      pure ()

unzip3PGArray :: [(a1, a2, a3)] -> (PGArray a1, PGArray a2, PGArray a3)
unzip3PGArray xs = xs & unzip3 & \(a, b, c) -> (PGArray a, PGArray b, PGArray c)

redactedGetTorrentFileAndInsert ::
  ( HasField "torrentId" r Int,
    MonadPostgres m,
    MonadThrow m,
    MonadLogger m,
    MonadOtel m
  ) =>
  r ->
  Transaction m (Label "torrentFile" ByteString)
redactedGetTorrentFileAndInsert dat = inSpan' "Redacted Get Torrent File and Insert" $ \span -> do
  bytes <- redactedGetTorrentFile dat
  execute
    [sql|
    UPDATE redacted.torrents_json
    SET torrent_file = ?::bytea
    WHERE torrent_id = ?::integer
  |]
    ( (Binary bytes :: Binary ByteString),
      dat.torrentId
    )
    >>= assertOneUpdated span "redactedGetTorrentFileAndInsert"
    >>= \() -> pure (label @"torrentFile" bytes)

getTorrentFileById ::
  ( MonadPostgres m,
    HasField "torrentId" r Int,
    MonadThrow m
  ) =>
  r ->
  Transaction m (Maybe (Label "torrentFile" ByteString))
getTorrentFileById dat = do
  queryWith
    [sql|
    SELECT torrent_file
    FROM redacted.torrents
    WHERE torrent_id = ?::integer
  |]
    (Only $ (dat.torrentId :: Int))
    (fmap @Maybe (label @"torrentFile") <$> Dec.byteaMay)
    >>= ensureSingleRow

updateTransmissionTorrentHashById ::
  ( MonadPostgres m,
    HasField "torrentId" r Int,
    HasField "torrentHash" r Text
  ) =>
  r ->
  Transaction m (Label "numberOfRowsAffected" Natural)
updateTransmissionTorrentHashById dat = do
  execute
    [sql|
    UPDATE redacted.torrents_json
    SET transmission_torrent_hash = ?::text
    WHERE torrent_id = ?::integer
    |]
    ( dat.torrentHash :: Text,
      dat.torrentId :: Int
    )

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

data TorrentData transmissionInfo = TorrentData
  { groupId :: Int,
    torrentId :: Int,
    seedingWeight :: Int,
    torrentJson :: Json.Value,
    torrentGroupJson :: T2 "artist" Text "groupName" Text,
    torrentStatus :: TorrentStatus transmissionInfo
  }

data TorrentStatus transmissionInfo
  = NoTorrentFileYet
  | NotInTransmissionYet
  | InTransmission (T2 "torrentHash" Text "transmissionInfo" transmissionInfo)

getTorrentById :: (MonadPostgres m, HasField "torrentId" r Int, MonadThrow m) => r -> Transaction m Json.Value
getTorrentById dat = do
  queryWith
    [sql|
    SELECT full_json_result FROM redacted.torrents
    WHERE torrent_id = ?::integer
  |]
    (getLabel @"torrentId" dat)
    (Dec.json Json.asValue)
    >>= ensureSingleRow

-- | Find the best torrent for each torrent group (based on the seeding_weight)
getBestTorrents :: (MonadPostgres m) => Transaction m [TorrentData ()]
getBestTorrents = do
  queryWith
    [sql|
      SELECT * FROM (
        SELECT DISTINCT ON (group_id)
          tg.group_id,
          t.torrent_id,
          seeding_weight,
          t.full_json_result AS torrent_json,
          tg.full_json_result AS torrent_group_json,
          t.torrent_file IS NOT NULL,
          t.transmission_torrent_hash
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
        hasTorrentFile <- Dec.fromField @Bool
        transmissionTorrentHash <-
          Dec.fromField @(Maybe Text)
        pure $
          TorrentData
            { torrentStatus =
                if
                  | not hasTorrentFile -> NoTorrentFileYet
                  | Nothing <- transmissionTorrentHash -> NotInTransmissionYet
                  | Just hash <- transmissionTorrentHash ->
                      InTransmission $
                        T2 (label @"torrentHash" hash) (label @"transmissionInfo" ()),
              ..
            }
    )

-- | Do a request to the redacted API. If you know what that is, you know how to find the API docs.
mkRedactedApiRequest ::
  ( MonadThrow m,
    MonadIO m,
    MonadLogger m,
    HasField "action" p ByteString,
    HasField "actionArgs" p [(ByteString, Maybe ByteString)]
  ) =>
  p ->
  m Http.Request
mkRedactedApiRequest dat = do
  authKey <- runCommandExpect0 "pass" ["internet/redacted/api-keys/whatcd-resolver"]
  pure $
    [fmt|https://redacted.ch/ajax.php|]
      & Http.setRequestMethod "GET"
      & Http.setQueryString (("action", Just dat.action) : dat.actionArgs)
      & Http.setRequestHeader "Authorization" [authKey]

httpGetJsonLd :: (MonadThrow m, MonadOtel m) => (URI, Http.Request) -> m Jsonld
httpGetJsonLd (uri, req) = inSpan' "Fetch json+ld" $ \span -> do
  addAttribute span "json+ld.targetUrl" (uri & showToText)
  httpJson
    (mkOptional (label @"contentType" "application/ld+json"))
    jsonldParser
    ( req
        & Http.setRequestMethod "GET"
        & Http.setRequestHeader "Accept" ["application/ld+json"]
    )

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

newtype Optional a = OptionalInternal (Maybe a)

mkOptional :: a -> Optional a
mkOptional defaultValue = OptionalInternal $ Just defaultValue

defaults :: Optional a
defaults = OptionalInternal Nothing

instance HasField "withDefault" (Optional a) (a -> a) where
  getField (OptionalInternal m) defaultValue = case m of
    Nothing -> defaultValue
    Just a -> a

httpJson ::
  ( MonadThrow m,
    MonadOtel m
  ) =>
  (Optional (Label "contentType" ByteString)) ->
  Json.Parse ErrorTree b ->
  Http.Request ->
  m b
httpJson opts parser req = inSpan' "HTTP Request (JSON)" $ \span -> do
  let opts' = opts.withDefault (label @"contentType" "application/json")
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
              Just ct <- contentType,
              ct == opts'.contentType ->
                Right $ (resp & Http.responseBody)
            | statusCode == 200,
              Just otherType <- contentType ->
                Left [fmt|Server returned a non-json body, with content-type "{otherType}"|]
            | statusCode == 200,
              Nothing <- contentType ->
                Left [fmt|Server returned a body with unspecified content type|]
            | code <- statusCode -> Left [fmt|Server returned an non-200 error code, code {code}: {resp & showPretty}|]
      )
    >>= assertM
      span
      ( \body ->
          Json.parseStrict parser body
            & first (Json.parseErrorTree "could not parse redacted response")
      )

redactedApiRequestJson ::
  ( MonadThrow m,
    MonadLogger m,
    HasField "action" p ByteString,
    HasField "actionArgs" p [(ByteString, Maybe ByteString)],
    MonadOtel m
  ) =>
  p ->
  Json.Parse ErrorTree a ->
  m a
redactedApiRequestJson dat parser =
  do
    mkRedactedApiRequest dat
    >>= httpJson defaults parser

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

class MonadTransmission m where
  getTransmissionId :: m (Maybe ByteString)
  setTransmissionId :: ByteString -> m ()

instance (MonadIO m) => MonadTransmission (AppT m) where
  getTransmissionId = AppT (asks (.transmissionSessionId)) >>= tryTakeMVar
  setTransmissionId t = do
    var <- AppT $ asks (.transmissionSessionId)
    putMVar var t
