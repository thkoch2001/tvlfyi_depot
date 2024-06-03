{-# LANGUAGE QuasiQuotes #-}

module Redacted where

import AppT
import Arg
import Control.Monad.Logger.CallStack
import Control.Monad.Reader
import Data.Aeson qualified as Json
import Data.Aeson.BetterErrors qualified as Json
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Error.Tree
import Data.List qualified as List
import Database.PostgreSQL.Simple (Binary (Binary), Only (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.Types (PGArray (PGArray))
import FieldParser qualified as Field
import Json qualified
import Label
import MyPrelude
import Network.HTTP.Client.Conduit qualified as Http
import Network.HTTP.Simple qualified as Http
import Network.HTTP.Types
import Network.Wai.Parse qualified as Wai
import OpenTelemetry.Trace qualified as Otel hiding (getTracer, inSpan, inSpan')
import Optional
import Postgres.Decoder qualified as Dec
import Postgres.MonadPostgres
import Pretty
import RunCommand (runCommandExpect0)
import Prelude hiding (span)

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

mkRedactedTorrentLink :: Arg "torrentId" Int -> Text
mkRedactedTorrentLink torrentId = [fmt|https://redacted.ch/torrents.php?id={torrentId.unArg}|]

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
      & \case
        IsNonEmpty tgs -> tgs & insertTourGroupsAndTorrents
        IsEmpty -> pure ()
  where
    go mpage =
      redactedSearch
        ( extraArguments
            -- pass the page (for every search but the first one)
            <> (mpage & ifExists (\page -> ("page", (page :: Natural) & showToText & textToBytesUtf8)))
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
      NonEmpty
        ( T2
            "tourGroup"
            (T3 "groupId" Int "groupName" Text "fullJsonResult" Json.Value)
            "torrents"
            [T2 "torrentId" Int "fullJsonResult" Json.Value]
        ) ->
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
                      (label @"torrents" (torrents & toList))
            )
    insertTourGroups ::
      NonEmpty
        ( T3
            "groupId"
            Int
            "groupName"
            Text
            "fullJsonResult"
            Json.Value
        ) ->
      Transaction m [Label "tourGroupIdPg" Int]
    insertTourGroups dats = do
      let groupNames =
            dats <&> \dat -> [fmt|{dat.groupId}: {dat.groupName}|]
      logInfo [fmt|Inserting tour groups for {showPretty groupNames}|]
      _ <-
        execute
          [fmt|
                  DELETE FROM redacted.torrent_groups
                  WHERE group_id = ANY (?::integer[])
              |]
          (Only $ (dats <&> (.groupId) & toList & PGArray :: PGArray Int))
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

data TorrentData transmissionInfo = TorrentData
  { groupId :: Int,
    torrentId :: Int,
    seedingWeight :: Int,
    artists :: [T2 "artistId" Int "artistName" Text],
    torrentJson :: Json.Value,
    torrentGroupJson :: TorrentGroupJson,
    torrentStatus :: TorrentStatus transmissionInfo
  }

data TorrentGroupJson = TorrentGroupJson
  { groupName :: Text,
    groupYear :: Int
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

data GetBestTorrentsFilter = GetBestTorrentsFilter
  { onlyDownloaded :: Bool,
    onlyArtist :: Maybe (Label "redactedId" Natural)
  }

-- | Find the best torrent for each torrent group (based on the seeding_weight)
getBestTorrents ::
  (MonadPostgres m) =>
  GetBestTorrentsFilter ->
  Transaction m [TorrentData ()]
getBestTorrents opts = do
  queryWith
    [sql|
      WITH filtered_torrents AS (
        SELECT DISTINCT ON (torrent_group)
          id
        FROM
          redacted.torrents
        WHERE
          -- onlyDownloaded
          ((NOT ?::bool) OR torrent_file IS NOT NULL)
          -- filter by artist id
          AND
          (?::bool OR (to_jsonb(?::int) <@ (jsonb_path_query_array(full_json_result, '$.artists[*].id'))))
        ORDER BY torrent_group, seeding_weight DESC
      )
      SELECT
        tg.group_id,
        t.torrent_id,
        t.seeding_weight,
        t.full_json_result AS torrent_json,
        tg.full_json_result AS torrent_group_json,
        t.torrent_file IS NOT NULL AS has_torrent_file,
        t.transmission_torrent_hash
      FROM filtered_torrents f
      JOIN redacted.torrents t ON t.id = f.id
      JOIN redacted.torrent_groups tg ON tg.id = t.torrent_group
      ORDER BY seeding_weight DESC
    |]
    ( do
        let (onlyArtistB, onlyArtistId) = case opts.onlyArtist of
              Nothing -> (True, 0)
              Just a -> (False, a.redactedId)
        ( opts.onlyDownloaded :: Bool,
          onlyArtistB :: Bool,
          onlyArtistId & fromIntegral @Natural @Int
          )
    )
    ( do
        groupId <- Dec.fromField @Int
        torrentId <- Dec.fromField @Int
        seedingWeight <- Dec.fromField @Int
        (torrentJson, artists) <- Dec.json $ do
          val <- Json.asValue
          artists <- Json.keyOrDefault "artists" [] $ Json.eachInArray $ do
            id_ <- Json.keyLabel @"artistId" "id" (Json.asIntegral @_ @Int)
            name <- Json.keyLabel @"artistName" "name" Json.asText
            pure $ T2 id_ name
          pure (val, artists)
        torrentGroupJson <-
          ( Dec.json $ do
              groupName <- Json.key "groupName" Json.asText
              groupYear <- Json.key "groupYear" (Json.asIntegral @_ @Int)
              pure $ TorrentGroupJson {..}
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
