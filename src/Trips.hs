{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
module Trips where
--------------------------------------------------------------------------------
import Database.SQLite.Simple
import Utils

import qualified Types as T
--------------------------------------------------------------------------------

-- | Create a new `trip` in `dbFile`.
create :: FilePath -> T.Trip -> IO ()
create dbFile trip = withConnection dbFile $ \conn ->
  execute conn "INSERT INTO Trips (username,destination,startDate,endDate,comment) VALUES (?,?,?,?,?)"
    (trip |> T.tripFields)

-- | Delete a trip from `dbFile` using its `tripPK` Primary Key.
delete :: FilePath -> T.TripPK -> IO ()
delete dbFile tripPK =
  withConnection dbFile $ \conn -> do
    execute conn "DELETE FROM Trips WHERE username = ? AND destination = ? and startDate = ?"
      (tripPK |> T.tripPKFields)

-- | Return a list of all of the trips in `dbFile`.
listAll :: FilePath -> IO [T.Trip]
listAll dbFile = withConnection dbFile $ \conn ->
  query_ conn "SELECT username,destination,startDate,endDate,comment FROM Trips"

-- | Return a list of all of the trips in `dbFile`.
list :: FilePath -> T.Username -> IO [T.Trip]
list dbFile username = withConnection dbFile $ \conn ->
  query conn "SELECT username,destination,startDate,endDate,comment FROM Trips WHERE username = ?"
    (Only username)
