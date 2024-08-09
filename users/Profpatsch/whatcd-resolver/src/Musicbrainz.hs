{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Musicbrainz where

import AppT
import Data.Aeson qualified as Json
import Data.Aeson.BetterErrors qualified as Json
import Data.Char qualified as Char
import Data.Text qualified as Text
import FieldParser (FieldParser)
import FieldParser qualified as Field
import Http qualified
import Label
import MyPrelude
import Optional

newtype MBId = MBId {unMBId :: Text}

mbId :: FieldParser Text MBId
mbId = Field.FieldParser $ \t ->
  if t
    & Text.all (\c -> Char.isAscii c || c == '-')
    then Right $ MBId t
    else Left [fmt|This is not a valid Musicbrainz ID: {t}|]

getMusicbrainz :: (MonadThrow m, MonadOtel m) => T2 "entity" Text "id" MBId -> m Json.Value
getMusicbrainz dat = inSpan' [fmt|Fetch musicbrainz {dat.entity} for {dat.id.unMBId}|] $ \span -> do
  req <-
    parseOrThrow
      span
      Http.parseRequest'
      [fmt|https://musicbrainz.org/ws/2/{dat.entity}/{dat.id.unMBId}|]
      <&> Http.fromRequestLiteral
      <&> Http.setRequestHeader "User-Agent" ["whatcd-resolver/unreleased ( mail@profpatsch.de )"]
      <&> Http.setQueryString
        [ ("fmt", Just "json"),
          ("inc", Just "release-group-rels+artist-credits"),
          ("offset", Just "0"),
          ("limit", Just "10")
        ]

  Http.httpJson
    defaults
    Json.asValue
    req
