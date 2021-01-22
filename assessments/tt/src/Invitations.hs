{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------------------
module Invitations where
--------------------------------------------------------------------------------
import Database.SQLite.Simple

import qualified Types as T
--------------------------------------------------------------------------------

create :: FilePath -> T.InvitationSecret -> T.Email -> T.Role -> IO ()
create dbFile secret email role = withConnection dbFile $ \conn -> do
  execute conn "INSERT INTO Invitations (email,role,secret) VALUES (?,?,?)"
    (email, role, secret)

get :: FilePath -> T.Email -> IO (Maybe T.Invitation)
get dbFile email = withConnection dbFile $ \conn -> do
  res <- query conn "SELECT email,role,secret FROM Invitations WHERE email = ?" (Only email)
  case res of
    [x] -> pure (Just x)
    _ -> pure Nothing
