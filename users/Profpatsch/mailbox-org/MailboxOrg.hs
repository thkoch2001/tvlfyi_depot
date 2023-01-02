{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Aeson (parseErrorTree)
import Control.Monad (replicateM)
import Data.Aeson qualified as Json
import Data.Aeson.BetterErrors qualified as Json
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as Char8
import Data.Error.Tree (prettyErrorTree)
import Data.List qualified as List
import Data.Map qualified as Map
import ExecHelpers
import GHC.Records (HasField (..))
import Label
import MyPrelude
import Network.HTTP.Conduit qualified as Client
import Network.HTTP.Simple qualified as Client
import Pretty
import System.Exit qualified as Exit
import System.Process qualified as Proc
import System.Random qualified as Random
import System.Random.Stateful qualified as Random
import Prelude hiding (log)

secret :: IO (T2 "email" ByteString "password" ByteString)
secret = do
  T2
    (label @"email" "mail@profpatsch.de")
    <$> (label @"password" <$> fromPass "email/mailbox.org")
  where
    fromPass name =
      Proc.readProcess "pass" [name] ""
        <&> stringToText
        <&> textToBytesUtf8
        <&> Char8.strip

progName :: Text
progName = "mailbox-org"

log :: Error -> IO ()
log err = do
  putStderrLn (errorContext progName err & prettyError)

main :: IO ()
main = run (CurrentProgramName progName) =<< secret

data MailfilterList = MailfilterList
  { id_ :: Json.Value,
    rulename :: Text
  }
  deriving stock (Show, Eq)

run ::
  ( HasField "email" dat ByteString,
    HasField "password" dat ByteString
  ) =>
  CurrentProgramName ->
  dat ->
  IO ()
run currentProg loginData = do
  session <- login loginData
  filters <-
    mailfilter
      session
      "list"
      ( Json.key "data" $ do
          ( Json.eachInArray $ asDat @"mailfilter" $ do
              id_ <- Json.key "id" Json.asValue
              rulename <- Json.key "rulename" Json.asText
              pure MailfilterList {..}
            )
            <&> mapFromListOn (\dat -> getLabel @"id_" dat.parsed)
      )
      ([] :: [()])
  filters
    & Map.elems
    & traverse_
      ( updateIfDifferent
          session
          ( \el ->
              pure $
                el.original.mailfilter
                  & KeyMap.insert "active" (Json.Bool False)
          )
          (pure ())
      )
  where
    mapFromListOn :: Ord k => (a -> k) -> [a] -> Map k a
    mapFromListOn on xs = xs <&> (\x -> (on x, x)) & Map.fromList
    updateIfDifferent ::
      forall label parsed.
      ( HasField "id_" parsed Json.Value,
        HasField "rulename" parsed Text
      ) =>
      Session ->
      (Dat label Json.Object parsed -> IO Json.Object) ->
      Json.Parse Error () ->
      Dat label Json.Object parsed ->
      IO ()
    updateIfDifferent session switcheroo parser dat = do
      new <- switcheroo dat
      if new /= getField @label dat.original
        then do
          log [fmt|Updating filter "{dat.parsed.rulename}" (id {dat.parsed.id_ & show @Json.Value})|]
          mailfilter session "update" parser new
        else do
          log [fmt|Skipping updating filter "{dat.parsed.rulename}" (id {dat.parsed.id_ & show @Json.Value}) because nothing changed.|]

    prettyRequestShort :: Client.Request -> Text
    prettyRequestShort req = [fmt|request {req & Client.method}: {req & Client.host}{req & Client.path}{req & Client.queryString}|]

    -- https://oxpedia.org/wiki/index.php?title=HTTP_API_MailFilter
    mailfilter session action parser body = do
      req <-
        Client.parseRequest "https://office.mailbox.org/appsuite/api/mailfilter/v2"
          <&> Client.setQueryString
            [ ("action", Just action),
              ("colums", Just "1")
            ]
          <&> Client.setRequestMethod "PUT"
          <&> Client.setRequestBodyJSON body
          <&> addSession session
      req
        & httpJSON currentProg [fmt|Cannot parse result for {req & prettyRequestShort}|] parser
        >>= okOrDie
        >>= (\resp -> printPretty resp >> pure resp)
        <&> Client.responseBody

newtype Session = Session Client.CookieJar

httpJSON ::
  CurrentProgramName ->
  Error ->
  Json.Parse Error b ->
  Client.Request ->
  IO (Client.Response b)
httpJSON currentProg errMsg parser req = do
  req
    & Client.httpJSON @_ @Json.Value
    >>= traverse
      ( \val -> do
          case val of
            Json.Object obj
              | "error" `KeyMap.member` obj
                  && "error_desc" `KeyMap.member` obj -> do
                  printPretty obj
                  diePanic currentProg "Server returned above inline error"
            _ -> pure ()
          val & Json.parseValue parser & \case
            Left errs ->
              errs
                & parseErrorTree errMsg
                & prettyErrorTree
                & diePanic currentProg
            Right a -> pure a
      )

data Dat label orig parsed = Dat
  { original :: Label label orig,
    parsed :: parsed
  }
  deriving stock (Show, Eq)

asDat ::
  forall label err m a.
  Monad m =>
  Json.ParseT err m a ->
  Json.ParseT err m (Dat label Json.Object a)
asDat parser = do
  original <- label @label <$> Json.asObject
  parsed <- parser
  pure Dat {..}

addSession :: Session -> Client.Request -> Client.Request
addSession (Session jar) req = do
  let sessionId =
        jar
          & Client.destroyCookieJar
          & List.find (\c -> "open-xchange-session-" `ByteString.isPrefixOf` c.cookie_name)
          & annotate "The cookie jar did not contain an open-exchange-session-*"
          & unwrapError
          & (.cookie_value)

  let req' = req & Client.addToRequestQueryString [("session", Just sessionId)]
  req' {Client.cookieJar = Just jar}

-- | Log into the mailbox.org service, and return the session secret cookies.
login :: (HasField "email" dat ByteString, HasField "password" dat ByteString) => dat -> IO Session
login dat = do
  rnd <- randomString
  req <-
    Client.parseRequest "https://office.mailbox.org/ajax/login"
      <&> Client.setQueryString
        [ ("action", Just "formlogin"),
          ("authId", Just $ ("mbo-" <> rnd) & stringToText & textToBytesUtf8)
        ]
      <&> Client.urlEncodedBody
        [ ("version", "Form+Login"),
          ("autologin", "true"),
          ("client", "open-xchange-appsuite"),
          ("uiWebPath", "/appsuite/"),
          ("login", dat.email),
          ("password", dat.password)
        ]
  Client.httpNoBody req
    >>= okOrDie
    <&> Client.responseCookieJar
    <&> Session
  where
    -- For some reason they want the client to pass a random string
    -- which is used for the session?‽!?
    randomString = do
      gen <- Random.newIOGenM =<< Random.newStdGen
      let chars = ['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9']
      let len = 11
      Random.uniformRM (0, List.length chars - 1) gen
        & replicateM len
        <&> map (\index -> chars !! index)

okOrDie :: Show a => Client.Response a -> IO (Client.Response a)
okOrDie resp =
  case resp & Client.getResponseStatusCode of
    200 -> pure resp
    _ -> do
      printPretty resp
      Exit.die "non-200 result"
