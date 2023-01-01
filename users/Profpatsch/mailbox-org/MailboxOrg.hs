{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Control.Monad (replicateM)
import Data.ByteString qualified as ByteString
import Data.List qualified as List
import MyPrelude
import Network.HTTP.Conduit qualified as Client
import Network.HTTP.Simple qualified as Client
import Pretty
import System.Exit qualified as Exit
import System.Random qualified as Random
import System.Random.Stateful qualified as Random
import Prelude hiding (log)
import Data.Aeson (Value)
import Label
import qualified System.Process as Proc
import qualified Data.ByteString.Char8 as Char8

secret :: IO (T2 "email" ByteString "password" ByteString)
secret = do
  T2
    (label @"email" "mail@profpatsch.de")
    <$> (label @"password" <$> fromPass "email/mailbox.org" )
  where
    fromPass name = Proc.readProcess "pass" [name] ""
     <&> stringToText <&> textToBytesUtf8
      <&> Char8.strip

main :: IO ()
main = run =<< secret


run :: (HasField "email" dat ByteString,
  HasField "password" dat ByteString) =>
 dat -> IO ()
run dat = do
  session <- login dat
  req <- Client.parseRequest "https://office.mailbox.org/appsuite/api/mailfilter/v2?action=list&columns=1"
    <&> Client.setRequestMethod "PUT"
    <&> addSession session
  Client.httpJSON @_ @Value req
    >>= okOrDie
    <&> Client.responseBody
    >>= printPretty

newtype Session = Session Client.CookieJar

addSession :: Session -> Client.Request -> Client.Request
addSession (Session jar) req = do
  let sessionId =
        jar
          & Client.destroyCookieJar
          & List.find (\c -> "open-xchange-session-" `ByteString.isPrefixOf` c.cookie_name)
          & annotate "The cookie jar did not contain an open-exchange-session-*"
          & unwrapError
          & (.cookie_value)
  (req
    & Client.addToRequestQueryString [("session", Just sessionId)])
      { Client.cookieJar = Just jar }

-- | Log into the mailbox.org service, and return the session secret cookies.
login ::
  (HasField "email" dat ByteString,
  HasField "password" dat ByteString) =>
  dat ->
  IO Session
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
