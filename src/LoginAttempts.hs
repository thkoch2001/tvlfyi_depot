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
  res <- query conn "SELECT (numAttempts) FROM LoginAttempts WHERE username = ?"
    (Only username)
  case res of
    [T.LoginAttempt{..}] -> pure (Just loginAttemptNumAttempts)
    _  -> pure Nothing

increment :: FilePath -> T.Username -> IO ()
increment dbFile username = withConnection dbFile $ \conn ->
  execute conn "UPDATE LoginAttempts SET numAttempts = numAttempts + 1 WHERE username = ?"
    (Only username)
