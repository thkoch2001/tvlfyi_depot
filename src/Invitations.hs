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
