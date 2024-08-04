{-# LANGUAGE QuasiQuotes #-}

module JsonLd where

import AppT
import Data.Aeson qualified as Json
import Data.Aeson.BetterErrors qualified as Json
import Data.ByteString.Builder qualified as Builder
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Html qualified
import Http
import IHP.HSX.QQ (hsx)
import Json qualified
import Label
import MyPrelude
import Network.HTTP.Client.Conduit qualified as Http
import Network.HTTP.Types.URI qualified as Url
import Network.URI (URI)
import Optional
import Text.Blaze.Html (Html)
import Prelude hiding (span)

-- | A recursive `json+ld` structure.
data Jsonld
  = JsonldObject JsonldObject
  | JsonldAnonymousObject JsonldAnonymousObject
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

-- | A json+ld object that cannot be inspected further by resolving its ID
data JsonldAnonymousObject = JsonldAnonymousObject'
  { -- | `@type` field; currently just the plain value without taking into account the json+ld context
    type_ :: Set Text,
    -- | fields of this anonymous object
    fields :: Map Text Jsonld
  }
  deriving stock (Show, Eq)

jsonldParser :: (Monad m) => Json.ParseT err m Jsonld
jsonldParser =
  Json.asValue >>= \cur -> do
    if
      | Json.Object _ <- cur -> do
          type_ <-
            Json.keyMay "@type" (Json.asArraySet Json.asText Json.<|> (Set.singleton <$> Json.asText))
              <&> fromMaybe Set.empty
          idMay <- Json.keyMay "@id" $ Json.asText
          fields <-
            Json.asObjectMap jsonldParser
              <&> Map.delete "@type"
              <&> Map.delete "@id"

          if
            | Just id_ <- idMay -> do
                pure $ JsonldObject $ JsonldObject' {previewFields = fields, ..}
            | otherwise -> pure $ JsonldAnonymousObject $ JsonldAnonymousObject' {..}
      | Json.Array _ <- cur -> do
          JsonldArray <$> Json.eachInArray jsonldParser
      | otherwise -> pure $ JsonldField cur

renderJsonld :: Jsonld -> Html
renderJsonld = \case
  JsonldObject obj -> renderObject obj (Just obj.id_) obj.previewFields
  JsonldAnonymousObject obj -> renderObject obj Nothing obj.fields
  JsonldArray arr ->
    Html.toOrderedList renderJsonld arr
  JsonldField f -> Html.mkVal f
  where
    renderObject obj mId_ fields = do
      let id_ =
            mId_ <&> \i ->
              [hsx|
                  <dt>Url</dt>
                  <dd><a href={i}>{i}</a></dd>
                  |]
          getMoreButton =
            mId_ <&> \i ->
              [hsx|
              <div>
                <button
                  hx-get={snippetHref i}
                  hx-target="closest dl"
                  hx-swap="outerHTML"
                >more fields …</button>
              </div>
            |]
      [hsx|
      <dl>
        <dt>Type</dt>
        <dd>{obj.type_ & toList & schemaTypes}</dd>
        {id_}
        <dt>Fields</dt>
        <dd>
          {fields & Html.toDefinitionList schemaType renderJsonld}
          {getMoreButton}
        </dd>
      </dl>
    |]
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
