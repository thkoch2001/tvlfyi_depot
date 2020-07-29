{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
--------------------------------------------------------------------------------
module App where
--------------------------------------------------------------------------------
import Control.Exception (throwIO)
import Control.Monad.IO.Class (liftIO)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Network.Wai.Handler.Warp as Warp
import Servant
import API
import Utils
import Web.Cookie

import qualified Crypto.KDF.BCrypt as BC
import qualified Data.Text.Encoding as TE
import qualified Types as T
import qualified Accounts as Accounts
import qualified Trips as Trips
import qualified Sessions as Sessions
import qualified LoginAttempts as LoginAttempts
--------------------------------------------------------------------------------

server :: FilePath -> Server API
server dbFile = createAccountH
           :<|> deleteAccountH
           :<|> listAccountsH
           :<|> createTripH
           :<|> deleteTripH
           :<|> listTripsH
           :<|> loginH
           :<|> logoutH
  where
    createAccountH newUser         = liftIO $ createAccount newUser
    deleteAccountH cookie username = liftIO $ deleteAccount cookie username
    listAccountsH cookie           = liftIO $ listAccounts cookie
    createTripH cookie trip        = liftIO $ createTrip cookie trip
    deleteTripH cookie tripPK      = liftIO $ deleteTrip cookie tripPK
    listTripsH                     = liftIO $ listTrips
    loginH creds                   = liftIO $ login creds
    logoutH cookie                 = liftIO $ logout cookie

    -- TODO(wpcarro): Handle failed CONSTRAINTs instead of sending 500s
    createAccount :: T.CreateAccountRequest -> IO NoContent
    createAccount request = do
      Accounts.create dbFile
        (T.createAccountRequestUsername request)
        (T.createAccountRequestPassword request)
        (T.createAccountRequestEmail request)
        (T.createAccountRequestRole request)
      pure NoContent

    deleteAccount :: T.SessionCookie -> Text -> IO NoContent
    deleteAccount cookie username = do
      Accounts.delete dbFile (T.Username username)
      pure NoContent

    listAccounts :: T.SessionCookie -> IO [T.User]
    listAccounts cookie = Accounts.list dbFile

    createTrip :: T.SessionCookie -> T.Trip -> IO NoContent
    createTrip cookie trip = do
      Trips.create dbFile trip
      pure NoContent

    -- TODO(wpcarro): Validate incoming data like startDate.
    deleteTrip :: T.SessionCookie -> T.TripPK -> IO NoContent
    deleteTrip cookie tripPK = do
      Trips.delete dbFile tripPK
      pure NoContent

    listTrips :: IO [T.Trip]
    listTrips = Trips.list dbFile

    login :: T.AccountCredentials
          -> IO (Headers '[Header "Set-Cookie" SetCookie] NoContent)
    login (T.AccountCredentials username password) = do
      mAccount <- Accounts.lookup dbFile username
      case mAccount of
        Just account@T.Account{..} -> do
          mAttempts <- LoginAttempts.forUsername dbFile accountUsername
          case mAttempts of
            Nothing ->
              if T.passwordsMatch password accountPassword then do
                session <- Sessions.findOrCreate dbFile account
                -- set cookie
                undefined
              else do
                LoginAttempts.increment dbFile username
                throwIO err401 { errBody = "Your credentials are invalid" }
            Just attempts ->
              if attempts > 3 then
                -- TODO(wpcarro): Prefer 429 error code
                throwIO err401 { errBody = "Too many failed login attempts" }
              else if T.passwordsMatch password accountPassword then do
                session <- Sessions.findOrCreate dbFile account
                -- set cookie
                undefined
              else do
                LoginAttempts.increment dbFile username
                -- TODO(wpcarro): Catch and return errors over HTTP
                throwIO err401 { errBody = "Your credentials are invalid" }

        -- In this branch, the user didn't supply a known username.
        Nothing -> throwIO err401 { errBody = "Your credentials are invalid" }

    logout :: T.SessionCookie
           -> IO (Headers '[Header "Set-Cookie" SetCookie] NoContent)
    logout cookie = undefined
      -- pull off SessionUUID from the request headers
      -- delete the SessionUUID from the Sessions table.

mkApp :: FilePath -> IO Application
mkApp dbFile = do
  pure $ serve (Proxy @ API) $ server dbFile

run :: FilePath -> IO ()
run sqliteFile =
  Warp.run 3000 =<< mkApp sqliteFile
