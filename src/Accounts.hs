{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
module Accounts where
--------------------------------------------------------------------------------
import Database.SQLite.Simple

import qualified PendingAccounts
import qualified Types as T
--------------------------------------------------------------------------------

-- | Delete the account in PendingAccounts and create on in Accounts.
transferFromPending :: FilePath -> T.PendingAccount -> IO ()
transferFromPending dbFile T.PendingAccount{..} = withConnection dbFile $
  \conn -> withTransaction conn $ do
    PendingAccounts.delete dbFile pendingAccountUsername
    execute conn "INSERT INTO Accounts (username,password,email,role) VALUES (?,?,?,?)"
      ( pendingAccountUsername
      , pendingAccountPassword
      , pendingAccountEmail
      , pendingAccountRole
      )

-- | Create a new account in the Accounts table.
create :: FilePath -> T.Username -> T.ClearTextPassword -> T.Email -> T.Role -> IO ()
create dbFile username password email role = withConnection dbFile $ \conn -> do
  hashed <- T.hashPassword password
  execute conn "INSERT INTO Accounts (username,password,email,role) VALUES (?,?,?,?)"
    (username, hashed, email, role)

-- | Delete `username` from `dbFile`.
delete :: FilePath -> T.Username -> IO ()
delete dbFile username = withConnection dbFile $ \conn -> do
  execute conn "DELETE FROM Accounts WHERE username = ?"
    (Only username)

-- | Attempt to find `username` in the Account table of `dbFile`.
lookup :: FilePath -> T.Username -> IO (Maybe T.Account)
lookup dbFile username = withConnection dbFile $ \conn -> do
  res <- query conn "SELECT username,password,email,role,profilePicture FROM Accounts WHERE username = ?" (Only username)
  case res of
    [x] -> pure (Just x)
    _ -> pure Nothing

-- | Return a list of accounts with the sensitive data removed.
list :: FilePath -> IO [T.User]
list dbFile = withConnection dbFile $ \conn -> do
  accounts <- query_ conn "SELECT username,password,email,role,profilePicture FROM Accounts"
  pure $ T.userFromAccount <$> accounts
