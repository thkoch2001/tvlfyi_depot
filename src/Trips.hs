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

-- | Attempt to get the trip record from `dbFile` under `tripKey`.
get :: FilePath -> T.TripPK -> IO (Maybe T.Trip)
get dbFile tripKey = withConnection dbFile $ \conn -> do
  res <- query conn "SELECT username,destination,startDate,endDate,comment FROM Trips WHERE username = ? AND destination = ? AND startDate = ? LIMIT 1"
    (T.tripPKFields tripKey)
  case res of
    [x] -> pure (Just x)
    _ -> pure Nothing

-- | Delete a trip from `dbFile` using its `tripKey` Primary Key.
delete :: FilePath -> T.TripPK -> IO ()
delete dbFile tripKey =
  withConnection dbFile $ \conn -> do
    execute conn "DELETE FROM Trips WHERE username = ? AND destination = ? and startDate = ?"
      (T.tripPKFields tripKey)

-- | Return a list of all of the trips in `dbFile`.
listAll :: FilePath -> IO [T.Trip]
listAll dbFile = withConnection dbFile $ \conn ->
  query_ conn "SELECT username,destination,startDate,endDate,comment FROM Trips ORDER BY date(startDate) ASC"

-- | Return a list of all of the trips in `dbFile`.
list :: FilePath -> T.Username -> IO [T.Trip]
list dbFile username = withConnection dbFile $ \conn ->
  query conn "SELECT username,destination,startDate,endDate,comment FROM Trips WHERE username = ? ORDER BY date(startDate) ASC"
    (Only username)
