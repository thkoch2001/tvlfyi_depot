{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------
module Sessions where
--------------------------------------------------------------------------------
import Data.Function ((&))
import Database.SQLite.Simple

import qualified Data.Time.Clock as Clock
import qualified Types as T
import qualified System.Random as Random
--------------------------------------------------------------------------------

-- | Return True if `session` was created at most three hours ago.
isValid :: T.StoredSession -> IO Bool
isValid session = do
  t1 <- Clock.getCurrentTime
  let t0 = T.storedSessionTsCreated session in
    pure $ Clock.diffUTCTime t1 t0 <= 3 * 60 * 60

-- | Lookup the session stored under `username` in `dbFile`.
find :: FilePath -> T.Username -> IO (Maybe T.StoredSession)
find dbFile username = withConnection dbFile $ \conn -> do
  res <- query conn "SELECT * FROM Sessions WHERE username = ?" (Only username)
  case res of
    [x] -> pure (Just x)
    _ -> pure Nothing

-- | Create a session under the `username` key in `dbFile`.
create :: FilePath -> T.Username -> IO T.SessionUUID
create dbFile username = withConnection dbFile $ \conn -> do
  now <- Clock.getCurrentTime
  uuid <- Random.randomIO
  execute conn "INSERT INTO Sessions (uuid,username,tsCreated) VALUES (?,?,?)"
    (T.SessionUUID uuid, username, now)
  pure (T.SessionUUID uuid)

-- | Reset the tsCreated field to the current time to ensure the token is valid.
refresh :: FilePath -> T.SessionUUID -> IO ()
refresh dbFile uuid = withConnection dbFile $ \conn -> do
  now <- Clock.getCurrentTime
  execute conn "UPDATE Sessions SET tsCreated = ? WHERE uuid = ?"
    (now, uuid)
  pure ()

-- | Delete the session under `username` from `dbFile`.
delete :: FilePath -> T.Username -> IO ()
delete dbFile username = withConnection dbFile $ \conn ->
  execute conn "DELETE FROM Sessions WHERE username = ?" (Only username)

-- | Find or create a session in the Sessions table. If a session exists,
-- refresh the token's validity.
findOrCreate :: FilePath -> T.Account -> IO T.SessionUUID
findOrCreate dbFile account = withConnection dbFile $ \conn ->
  let username = T.accountUsername account in do
    mSession <- find dbFile username
    case mSession of
      Nothing -> create dbFile username
      Just session ->
        let uuid = T.storedSessionUUID session in do
          refresh dbFile uuid
          pure uuid

-- | Return a list of all sessions in the Sessions table.
list :: FilePath -> IO [T.StoredSession]
list dbFile = withConnection dbFile $ \conn ->
  query_ conn "SELECT * FROM Sessions"
