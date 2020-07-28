{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
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

import qualified Crypto.KDF.BCrypt as BC
import qualified Data.Text.Encoding as TE
import qualified Types as T
import qualified Accounts as Accounts
import qualified Trips as Trips
--------------------------------------------------------------------------------

server :: FilePath -> Server API
server dbFile = createAccountH
           :<|> deleteAccountH
           :<|> listAccountsH
           :<|> createTripH
           :<|> deleteTripH
           :<|> listTripsH
           :<|> loginH
  where
    createAccountH newUser  = liftIO $ createAccount newUser
    deleteAccountH username = liftIO $ deleteAccount username
    listAccountsH           = liftIO $ listAccounts
    createTripH trip        = liftIO $ createTrip trip
    deleteTripH tripPK      = liftIO $ deleteTrip tripPK
    listTripsH              = liftIO $ listTrips
    loginH creds            = liftIO $ login creds

    -- TODO(wpcarro): Handle failed CONSTRAINTs instead of sending 500s
    createAccount :: T.CreateAccountRequest -> IO NoContent
    createAccount request = do
      Accounts.create dbFile
        (T.createAccountRequestUsername request)
        (T.createAccountRequestPassword request)
        (T.createAccountRequestEmail request)
        (T.createAccountRequestRole request)
      pure NoContent

    deleteAccount :: Text -> IO NoContent
    deleteAccount username = do
      Accounts.delete dbFile (T.Username username)
      pure NoContent

    listAccounts :: IO [T.User]
    listAccounts = Accounts.list dbFile

    createTrip :: T.Trip -> IO NoContent
    createTrip trip = do
      Trips.create dbFile trip
      pure NoContent

    listTrips :: IO [T.Trip]
    listTrips = Trips.list dbFile

    -- TODO(wpcarro): Validate incoming data like startDate.
    deleteTrip :: T.TripPK -> IO NoContent
    deleteTrip tripPK = do
      Trips.delete dbFile tripPK
      pure NoContent

    -- TODO(wpcarro): Create and store a session token
    login :: T.AccountCredentials -> IO (Maybe T.Session)
    login (T.AccountCredentials username password) =
      withConnection dbFile $ \conn -> do
        res <- query conn "SELECT * FROM Accounts WHERE username = ?"
          (Only username)
        case res of
          [T.Account{T.accountUsername,T.accountPassword,T.accountRole}] ->
            if T.passwordsMatch password accountPassword then
              pure $ Just (T.Session accountUsername accountRole)
            else
              -- TODO(wpcarro): Catch and return errors over HTTP
              throwIO $ err401 { errBody = "Your credentials are invalid" }

          -- In this branch, the user didn't supply a known username.
          _ -> throwIO $ err401 { errBody = "Your credentials are invalid" }

mkApp :: FilePath -> IO Application
mkApp dbFile = do
  pure $ serve (Proxy @ API) $ server dbFile

run :: FilePath -> IO ()
run sqliteFile =
  Warp.run 3000 =<< mkApp sqliteFile
