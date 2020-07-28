{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
--------------------------------------------------------------------------------
module App where
--------------------------------------------------------------------------------
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStderrLoggingT)
import Data.Function ((&))
import Data.String.Conversions (cs)
import Data.Text (Text)
import Database.SQLite.Simple
import Network.Wai.Handler.Warp as Warp
import Servant

import API
import qualified Types as T
--------------------------------------------------------------------------------

server :: FilePath -> Server API
server dbFile = createAccountH
           :<|> deleteAccountH
           :<|> listAccountsH
           :<|> createTripH
           :<|> deleteTripH
           :<|> listTripsH
  where
    createAccountH newUser  = liftIO $ createAccount newUser
    deleteAccountH username = liftIO $ deleteAccount username
    listAccountsH           = liftIO $ listAccounts

    createTripH trip        = liftIO $ createTrip trip
    deleteTripH tripPK      = liftIO $ deleteTrip tripPK
    listTripsH              = liftIO $ listTrips

    -- TODO(wpcarro): Handle failed CONSTRAINTs instead of sending 500s
    createAccount :: T.Account -> IO (Maybe T.Session)
    createAccount account = withConnection dbFile $ \conn -> do
      execute conn "INSERT INTO Accounts (username,password,email,role,profilePicture) VALUES (?,?,?,?,?)"
        (account & T.accountFields)
      T.Session{ T.username = T.accountUsername account
               , T.password = T.accountPassword account
               , T.role = T.accountRole account
               } & Just & pure

    deleteAccount :: Text -> IO NoContent
    deleteAccount username = withConnection dbFile $ \conn -> do
      execute conn "DELETE FROM Accounts WHERE username = ?"
        (Only (T.Username username))
      pure NoContent

    listAccounts :: IO [T.User]
    listAccounts = withConnection dbFile $ \conn -> do
      accounts <- query_ conn "SELECT * FROM Accounts"
      pure $ T.userFromAccount <$> accounts

    createTrip :: T.Trip -> IO NoContent
    createTrip trip = withConnection dbFile $ \conn -> do
      execute conn "INSERT INTO Trips (username,destination,startDate,endDate,comment) VALUES (?,?,?,?,?)"
        (trip & T.tripFields)
      pure NoContent

    listTrips :: IO [T.Trip]
    listTrips = withConnection dbFile $ \conn ->
      query_ conn "SELECT * FROM Trips"

    -- TODO(wpcarro): Validate incoming data like startDate.
    deleteTrip :: T.TripPK -> IO NoContent
    deleteTrip tripPK =
      withConnection dbFile $ \conn -> do
        execute conn "DELETE FROM Trips WHERE username = ? AND destination = ? and startDate = ?"
          (tripPK & T.tripPKFields)
        pure NoContent

mkApp :: FilePath -> IO Application
mkApp dbFile = do
  pure $ serve (Proxy @ API) $ server dbFile

run :: FilePath -> IO ()
run sqliteFile =
  Warp.run 3000 =<< mkApp sqliteFile
