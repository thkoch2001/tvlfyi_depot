{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------------------
module Auth where
--------------------------------------------------------------------------------
import Database.SQLite.Simple
import Utils
import Web.Cookie

import qualified Data.UUID as UUID
import qualified Web.Cookie as WC
import qualified Sessions as Sessions
import qualified Accounts as Accounts
import qualified Types as T
import qualified Data.ByteString.Lazy as LBS
--------------------------------------------------------------------------------

-- | Return the UUID from a Session cookie.
uuidFromCookie :: T.SessionCookie -> Maybe T.SessionUUID
uuidFromCookie (T.SessionCookie cookies) = do
  auth <- lookup "auth" cookies
  uuid <- UUID.fromASCIIBytes auth
  pure $ T.SessionUUID uuid

-- | Attempt to return the user role associated with the `cookie`.
roleFromCookie :: FilePath -> T.SessionCookie -> IO (Maybe T.Role)
roleFromCookie dbFile cookie = withConnection dbFile $ \conn -> do
  case uuidFromCookie cookie of
    Nothing -> pure Nothing
    Just uuid -> do
      mSession <- Sessions.get dbFile uuid
      case mSession of
        Nothing -> pure Nothing
        Just T.StoredSession{..} -> do
          mAccount <- Accounts.lookup dbFile storedSessionUsername
          case mAccount of
            Nothing -> pure Nothing
            Just T.Account{..} -> pure (Just accountRole)

-- | Create a new session cookie.
mkCookie :: T.SessionUUID -> SetCookie
mkCookie (T.SessionUUID uuid) =
  defaultSetCookie
    { setCookieName = "auth"
    , setCookieValue = UUID.toASCIIBytes uuid
    }

-- | Use this to clear out the session cookie.
emptyCookie :: SetCookie
emptyCookie =
  defaultSetCookie
    { setCookieName = "auth"
    , setCookieValue = ""
    }
