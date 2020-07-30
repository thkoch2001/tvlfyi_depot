{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------------------
module LoginAttempts where
--------------------------------------------------------------------------------
import Database.SQLite.Simple

import qualified Types as T
--------------------------------------------------------------------------------

reset :: FilePath -> T.Username -> IO ()
reset dbFile username = withConnection dbFile $ \conn ->
  execute conn "UPDATE LoginAttempts SET numAttempts = 0 WHERE username = ?"
    (Only username)

-- | Attempt to return the number of failed login attempts for
-- `username`. Returns a Maybe in case `username` doesn't exist.
forUsername :: FilePath -> T.Username -> IO (Maybe Integer)
forUsername dbFile username = withConnection dbFile $ \conn -> do
  res <- query conn "SELECT username,numAttempts FROM LoginAttempts WHERE username = ?"
    (Only username)
  case res of
    [T.LoginAttempt{..}] -> pure (Just loginAttemptNumAttempts)
    _  -> pure Nothing

-- | INSERT a failed login attempt for `username` or UPDATE an existing entry.
increment :: FilePath -> T.Username -> IO ()
increment dbFile username = withConnection dbFile $ \conn ->
  execute conn "INSERT INTO LoginAttempts (username,numAttempts) VALUES (?,?) ON CONFLICT (username) DO UPDATE SET numAttempts = numAttempts + 1"
    (username, 1 :: Integer)
