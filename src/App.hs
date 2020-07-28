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
server dbFile = userAddH
           :<|> userGetH
           :<|> createTripH
           :<|> listTripsH
           :<|> deleteTripH
  where
    userAddH newUser   = liftIO $ userAdd newUser
    userGetH name      = liftIO $ userGet name
    createTripH trip   = liftIO $ createTrip trip
    listTripsH         = liftIO $ listTrips
    deleteTripH tripPK = liftIO $ deleteTrip tripPK

    -- TODO(wpcarro): Handle failed CONSTRAINTs instead of sending 500s
    userAdd :: T.Account -> IO (Maybe T.Session)
    userAdd account = withConnection dbFile $ \conn -> do
      execute conn "INSERT INTO Accounts (username,password,email,role,profilePicture) VALUES (?,?,?,?,?)"
        (account & T.accountFields)
      T.Session{ T.username = T.accountUsername account
               , T.password = T.accountPassword account
               , T.role = T.accountRole account
               } & Just & pure

    userGet :: Text -> IO (Maybe T.Account)
    userGet name = withConnection dbFile $ \conn -> do
      res <- query conn "SELECT * FROM Accounts WHERE username = ?" (Only name)
      case res of
        [x] -> pure (Just x)
        _   -> pure Nothing

    createTrip :: T.Trip -> IO NoContent
    createTrip trip = withConnection dbFile $ \conn -> do
      execute conn "INSERT INTO Trips (username,destination,startDate,endDate,comment) VALUES (?,?,?,?,?)"
        (trip & T.tripFields)
      pure NoContent

    listTrips :: IO [T.Trip]
    listTrips = withConnection dbFile $ \conn -> do
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
