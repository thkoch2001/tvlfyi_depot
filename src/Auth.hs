{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------------------
module Auth where
--------------------------------------------------------------------------------
import Control.Monad.IO.Class (liftIO)
import Data.String.Conversions (cs)
import Database.SQLite.Simple
import Utils
import Web.Cookie
import Servant
import Servant.Server.Internal.ServerError

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

-- | Attempt to return the account associated with `cookie`.
accountFromCookie :: FilePath -> T.SessionCookie -> IO (Maybe T.Account)
accountFromCookie dbFile cookie = withConnection dbFile $ \conn -> do
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
            Just x -> pure (Just x)

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

-- | Throw a 401 error if the `predicate` fails.
assert :: FilePath -> T.SessionCookie -> (T.Account -> Bool) -> Handler a -> Handler a
assert dbFile cookie predicate handler = do
  mRole <- liftIO $ accountFromCookie dbFile cookie
  case mRole of
    Nothing -> throwError err401 { errBody = "Missing valid session cookie" }
    Just account ->
      if predicate account then
        handler
      else
        throwError err401 { errBody = "You are not authorized to access this resource" }
