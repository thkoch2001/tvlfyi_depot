{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------------------
module PendingAccounts where
--------------------------------------------------------------------------------
import Database.SQLite.Simple

import qualified Types as T
--------------------------------------------------------------------------------

create :: FilePath
       -> T.RegistrationSecret
       -> T.Username
       -> T.ClearTextPassword
       -> T.Role
       -> T.Email
       -> IO ()
create dbFile secret username password role email = withConnection dbFile $ \conn -> do
  hashed <- T.hashPassword password
  execute conn "INSERT INTO PendingAccounts (secret,username,password,role,email) VALUES (?,?,?,?,?)"
    (secret, username, hashed, role, email)

get :: FilePath -> T.Username -> IO (Maybe T.PendingAccount)
get dbFile username = withConnection dbFile $ \conn -> do
  res <- query conn "SELECT secret,username,password,role,email FROM PendingAccounts WHERE username = ?" (Only username)
  case res of
    [x] -> pure (Just x)
    _ -> pure Nothing

delete :: FilePath -> T.Username -> IO ()
delete dbFile username = withConnection dbFile $ \conn ->
  execute conn "DELETE FROM PendingAccounts WHERE username = ?" (Only username)
