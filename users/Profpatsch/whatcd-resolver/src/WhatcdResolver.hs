{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}

module WhatcdResolver where

import Control.Category qualified as Cat
import Control.Monad.Logger qualified as Logger
import Control.Monad.Logger.CallStack
import Control.Monad.Reader
import Data.Aeson qualified as Json
import Data.Aeson.BetterErrors qualified as Json
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Error.Tree
import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Pool (Pool)
import Data.Pool qualified as Pool
import Data.Scientific (Scientific)
import Data.Text qualified as Text
import Database.PostgreSQL.Simple (Binary (Binary), Only (..))
import Database.PostgreSQL.Simple qualified as Postgres
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.Types (PGArray (PGArray))
import Database.Postgres.Temp qualified as TmpPg
import FieldParser (FieldParser' (..))
import FieldParser qualified as Field
import GHC.Records (HasField (..))
import IHP.HSX.QQ (hsx)
import Json qualified
import Json.Enc (Enc)
import Json.Enc qualified as Enc
import Label
import Multipart2 qualified as Multipart
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
import RunCommand (runCommandExpect0)
import System.Directory qualified as Dir
import System.Directory qualified as Xdg
import System.FilePath ((</>))
import System.IO qualified as IO
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Pretty qualified as Html.Pretty
import Text.Blaze.Html.Renderer.Utf8 qualified as Html
import Text.Blaze.Html5 qualified as Html
import Tool (Tool, readTool, readTools)
import UnliftIO

htmlUi :: App ()
htmlUi = do
  let debug = True
  withRunInIO $ \runInIO -> Warp.run 8080 $ \req respond -> do
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
      let h act = do
            res <- runInIO act
            respond . Wai.responseLBS Http.ok200 [("Content-Type", "text/html")] . renderHtml $ res

      let mp parser =
            Multipart.parseMultipartOrThrow
              appThrowTree
              parser
              req

      let torrentIdMp =
            mp
              ( do
                  label @"torrentId" <$> Multipart.field "torrent-id" ((Field.utf8 >>> Field.signedDecimal >>> Field.bounded @Int "int"))
              )

      case req & Wai.pathInfo & Text.intercalate "/" of
        "" -> h mainHtml
        "snips/redacted/search" -> do
          h $ do
            dat <-
              mp
                ( do
                    label @"searchstr" <$> Multipart.field "redacted-search" Cat.id
                )
            snipsRedactedSearch dat
        "snips/redacted/torrentDataJson" -> h $ do
          dat <- torrentIdMp
          mkVal <$> (runTransaction $ getTorrentById dat)
        "snips/redacted/getTorrentFile" -> h $ do
          dat <- torrentIdMp
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
        "snips/redacted/startTorrentFile" -> h $ do
          dat <- torrentIdMp
          runTransaction $ do
            file <- getTorrentFileById dat
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
        "snips/transmission/getTorrentState" -> h $ do
          dat <- mp $ label @"torrentHash" <$> Multipart.field "torrent-hash" Field.utf8
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
        _ -> h mainHtml
  where
    everySecond :: Text -> Enc -> Html -> Html
    everySecond call extraData innerHtml = [hsx|<div hx-trigger="every 1s" hx-swap="outerHTML" hx-post={call} hx-vals={Enc.encToBytesUtf8 extraData}>{innerHtml}</div>|]

    mainHtml = runTransaction $ do
      bestTorrentsTable <- getBestTorrentsTable
      transmissionTorrentsTable <- lift @Transaction getTransmissionTorrentsTable
      pure $
        Html.docTypeHtml
          [hsx|
      <head>
        <meta charset="utf-8">
        <meta name="viewport" content="width=device-width, initial-scale=1">
        <link href="https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/css/bootstrap.min.css" rel="stylesheet" integrity="sha384-9ndCyUaIbzAi2FUVXJi0CjmCapSmO7SnpJef0486qhLnuZ2cdeRhO02iuK6FUUVM" crossorigin="anonymous">
        <script src="https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/js/bootstrap.bundle.min.js" integrity="sha384-geWF76RCwLtnZ8qwWowPQNguL3RmwHVBC9FhGdlKrxdiJJigb/j/68SIy3Te4Bkz" crossorigin="anonymous"></script>
        <script src="https://unpkg.com/htmx.org@1.9.2" integrity="sha384-L6OqL9pRWyyFU3+/bjdSri+iIphTN/bvYyM37tICVyOJkWZLpP2vGn6VUEXgzg6h" crossorigin="anonymous"></script>
      </head>
      <body>
        <form
          hx-post="/snips/redacted/search"
          hx-target="#redacted-search-results">
          <label for="redacted-search">Redacted Search</label>
          <input
            id="redacted-search"
            type="text"
            name="redacted-search" />
          <button type="submit">Search</button>
        </form>
        <div id="redacted-search-results">
          {bestTorrentsTable}
        </div>
        <div id="transmission-torrents">
          {transmissionTorrentsTable}
        </div>
      </body>
    |]

snipsRedactedSearch ::
  ( MonadLogger m,
    MonadIO m,
    MonadPostgres m,
    HasField "searchstr" r ByteString,
    MonadThrow m,
    MonadTransmission m
  ) =>
  r ->
  m Html
snipsRedactedSearch dat = do
  t <-
    redactedSearchAndInsert
      [ ("searchstr", dat.searchstr)
      ]
  runTransaction $ do
    t
    getBestTorrentsTable

getBestTorrentsTable :: (MonadIO m, MonadTransmission m, MonadThrow m, MonadLogger m, MonadPostgres m) => Transaction m Html
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
                  NoTorrentFileYet -> td {torrentStatus = NotInTransmissionYet}
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
  (MonadIO m, MonadTransmission m, MonadThrow m, MonadLogger m, MonadPostgres m) =>
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
              percentDone <- Json.keyLabel @"percentDone" "percentDone" (Field.jsonParser $ Field.jsonNumber >>> scientificPercentage)
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
  (MonadIO m, MonadTransmission m, MonadThrow m, MonadLogger m) =>
  m Html
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
      toTable
        ( resp
            & List.sortOn (\m -> m & Map.lookup "percentDone" & fromMaybe (Json.Number 0))
            <&> Map.toList
            -- TODO
            & List.take 100
        )

-- | Render an arbitrary json value to HTML in a more-or-less reasonable fashion.
mkVal :: Json.Value -> Html
mkVal = \case
  Json.Number n -> Html.toHtml @Text $ showToText n
  Json.String s -> Html.toHtml @Text s
  Json.Bool True -> [hsx|<em>true</em>|]
  Json.Bool False -> [hsx|<em>false</em>|]
  Json.Null -> [hsx|<em>null</em>|]
  Json.Array arr ->
    arr
      & foldMap (\el -> Html.li $ mkVal el)
      & Html.ol
  Json.Object obj ->
    obj
      & KeyMap.toMapText
      & Map.toList
      & foldMap (\(k, v) -> Html.dt (Html.toHtml @Text k) <> Html.dd (mkVal v))
      & Html.dl

-- | Render a table-like structure of json values as an HTML table.
toTable :: [[(Text, Json.Value)]] -> Html
toTable xs =
  case xs & nonEmpty of
    Nothing ->
      [hsx|<p>No results.</p>|]
    Just xs' -> do
      let headers = xs' & NonEmpty.head <&> fst <&> (\h -> [hsx|<th>{h}</th>|]) & mconcat
      let vals = xs' & foldMap (Html.tr . foldMap (Html.td . mkVal . snd))
      [hsx|
              <table class="table">
                <thead>
                  <tr>
                  {headers}
                  </tr>
                </thead>
                <tbody>
                  {vals}
                </tbody>
              </table>
          |]

data TransmissionRequest = TransmissionRequest
  { method :: Text,
    arguments :: Map Text Enc,
    tag :: Maybe Int
  }
  deriving stock (Show)

testTransmission :: Show out => (TransmissionRequest, Json.Parse Error out) -> IO (Either TmpPg.StartError ())
testTransmission req = runAppWith $ doTransmissionRequest transmissionConnectionConfig req >>= liftIO . printPretty

transmissionConnectionConfig :: T2 "host" Text "port" Text
transmissionConnectionConfig = (T2 (label @"host" "localhost") (label @"port" "9091"))

transmissionRequestListAllTorrents :: Monad m => [Text] -> Json.ParseT e m out -> (TransmissionRequest, Json.ParseT e m [out])
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
              ("paused", Enc.bool True)
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
  ( MonadIO m,
    MonadTransmission m,
    MonadThrow m,
    MonadLogger m
  ) =>
  (TransmissionRequest, Json.Parse Error output) ->
  m output
doTransmissionRequest' req = do
  resp <-
    doTransmissionRequest
      transmissionConnectionConfig
      req
  case resp.result of
    TransmissionResponseFailure err -> appThrowTree (nestedError "Transmission RPC error" $ singleError $ newError err)
    TransmissionResponseSuccess -> case resp.arguments of
      Nothing -> appThrowTree "Transmission RPC error: No `arguments` field in response"
      Just out -> pure out

-- | Contact the transmission RPC, and do the CSRF protection dance.
--
-- Spec: https://github.com/transmission/transmission/blob/main/docs/rpc-spec.md
doTransmissionRequest ::
  ( MonadIO m,
    MonadTransmission m,
    HasField "host" t1 Text,
    HasField "port" t1 Text,
    MonadThrow m,
    MonadLogger m
  ) =>
  t1 ->
  (TransmissionRequest, Json.Parse Error output) ->
  m (TransmissionResponse output)
doTransmissionRequest dat (req, parser) = do
  sessionId <- getTransmissionId
  let body =
        Enc.object
          ( [ ("method", req.method & Enc.text),
              ("arguments", Enc.map id req.arguments)
            ]
              <> (req.tag & maybe [] (\t -> [("tag", t & Enc.int)]))
          )
  logDebug [fmt|transmission request: {Pretty.showPrettyJsonEncoding body.unEnc}|]
  let httpReq =
        [fmt|http://{dat.host}:{dat.port}/transmission/rpc|]
          & Http.setRequestMethod "POST"
          & Http.setRequestBodyLBS (Enc.encToBytesUtf8Lazy body)
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
      doTransmissionRequest dat (req, parser)
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
                  (Field.jsonParser (Field.jsonNumber >>> Field.boundedScientificIntegral "tag too long"))
              pure TransmissionResponse {..}
          )
        & first (Json.parseErrorTree "Cannot parse transmission RPC response")
        & \case
          Right a -> pure a
          Left err -> do
            case Json.eitherDecodeStrict' @Json.Value (resp & Http.getResponseBody) of
              Left _err -> pure ()
              Right val -> logInfo [fmt|failing transmission response: {showPrettyJson val}|]
            appThrowTree err
    _ -> liftIO $ unwrapIOError $ Left [fmt|Non-200 response: {showPretty resp}|]

redactedSearch ::
  (MonadLogger m, MonadIO m, MonadThrow m) =>
  [(ByteString, ByteString)] ->
  Json.Parse ErrorTree a ->
  m a
redactedSearch advanced =
  redactedApiRequestJson
    ( T2
        (label @"action" "browse")
        (label @"actionArgs" ((advanced <&> second Just)))
    )

redactedGetTorrentFile ::
  ( MonadLogger m,
    MonadIO m,
    MonadThrow m,
    HasField "torrentId" dat Int
  ) =>
  dat ->
  m ByteString
redactedGetTorrentFile dat =
  redactedApiRequest
    ( T2
        (label @"action" "download")
        (label @"actionArgs" [("id", Just (dat.torrentId & showToText @Int & textToBytesUtf8))])
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

redactedSearchAndInsert ::
  ( MonadLogger m,
    MonadIO m,
    MonadPostgres m,
    MonadThrow m
  ) =>
  [(ByteString, ByteString)] ->
  m (Transaction m ())
redactedSearchAndInsert x =
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

redactedGetTorrentFileAndInsert ::
  ( HasField "torrentId" r Int,
    MonadPostgres m,
    MonadThrow m,
    MonadIO m,
    MonadLogger m
  ) =>
  r ->
  Transaction m (Label "torrentFile" ByteString)
redactedGetTorrentFileAndInsert dat = do
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
    >>= assertOneUpdated "redactedGetTorrentFileAndInsert"
    >>= \() -> pure (label @"torrentFile" bytes)

getTorrentFileById ::
  ( MonadPostgres m,
    HasField "torrentId" r Int,
    MonadThrow m
  ) =>
  r ->
  Transaction m (Label "torrentFile" ByteString)
getTorrentFileById dat = do
  queryWith
    [sql|
    SELECT torrent_file
    FROM redacted.torrents
    WHERE torrent_id = ?::integer
  |]
    (Only $ (dat.torrentId :: Int))
    (label @"torrentFile" <$> Dec.bytea)
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
  (HasField "numberOfRowsAffected" r Natural, MonadThrow m) =>
  Text ->
  r ->
  m ()
assertOneUpdated name x = case x.numberOfRowsAffected of
  1 -> pure ()
  n -> appThrowTree ([fmt|{name :: Text}: Expected to update exactly one row, but updated {n :: Natural} row(s)|])

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
getBestTorrents :: MonadPostgres m => Transaction m [TorrentData ()]
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

-- | Do a request to the redacted API. If you know what that is, you know how to find the API docs.
redactedApiRequest ::
  ( MonadThrow m,
    MonadIO m,
    MonadLogger m,
    HasField "action" p ByteString,
    HasField "actionArgs" p [(ByteString, Maybe ByteString)]
  ) =>
  p ->
  m ByteString
redactedApiRequest dat = do
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

redactedApiRequestJson ::
  ( MonadThrow m,
    MonadIO m,
    MonadLogger m,
    HasField "action" p ByteString,
    HasField "actionArgs" p [(ByteString, Maybe ByteString)]
  ) =>
  p ->
  Json.Parse ErrorTree a ->
  m a
redactedApiRequestJson dat parse = do
  redactedApiRequest dat
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
  pgFormat <- readTools (label @"toolsEnvVar" "WHATCD_RESOLVER_TOOLS") (readTool "pg_format")
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
  { config :: Label "logDatabaseQueries" DebugLogDatabaseQueries,
    pgFormat :: Tool,
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

orAppThrowTree :: MonadThrow m => Either ErrorTree a -> m a
orAppThrowTree = \case
  Left err -> appThrowTree err
  Right a -> pure a

instance MonadIO m => MonadLogger (AppT m) where
  monadLoggerLog loc src lvl msg = liftIO $ Logger.defaultOutput IO.stderr loc src lvl (Logger.toLogStr msg)

class MonadTransmission m where
  getTransmissionId :: m (Maybe ByteString)
  setTransmissionId :: ByteString -> m ()

instance (MonadIO m) => MonadTransmission (AppT m) where
  getTransmissionId = AppT (asks (.transmissionSessionId)) >>= tryTakeMVar
  setTransmissionId t = do
    var <- AppT $ asks (.transmissionSessionId)
    putMVar var t

instance (MonadThrow m, MonadUnliftIO m) => MonadPostgres (AppT m) where
  execute = executeImpl (AppT ask) (AppT $ asks (.config.logDatabaseQueries))
  execute_ = executeImpl_ (AppT ask) (AppT $ asks (.config.logDatabaseQueries))
  executeMany = executeManyImpl (AppT ask) (AppT $ asks (.config.logDatabaseQueries))
  executeManyReturningWith = executeManyReturningWithImpl (AppT ask) (AppT $ asks (.config.logDatabaseQueries))
  queryWith = queryWithImpl (AppT ask) (AppT $ asks (.config.logDatabaseQueries))
  queryWith_ = queryWithImpl_ (AppT ask)
  foldRows = foldRowsImpl (AppT ask)
  runTransaction = runPGTransaction

runPGTransaction :: MonadUnliftIO m => Transaction (AppT m) a -> AppT m a
runPGTransaction (Transaction transaction) = do
  pool <- AppT ask <&> (.pgConnPool)
  withRunInIO $ \unliftIO ->
    withPGTransaction pool $ \conn -> do
      unliftIO $ runReaderT transaction conn

data HasQueryParams param
  = HasNoParams
  | HasSingleParam param
  | HasMultiParams [param]
