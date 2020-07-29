{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
--------------------------------------------------------------------------------
module API where
--------------------------------------------------------------------------------
import Data.Text
import Servant.API
import Web.Cookie

import qualified Types as T
--------------------------------------------------------------------------------

-- | Once authenticated, users receive a SessionCookie.
type SessionCookie = Header' '[Required] "Set-Cookie" T.SessionCookie

type API =
      -- accounts: Create
           "accounts"
           :> ReqBody '[JSON] T.CreateAccountRequest
           :> Post '[JSON] NoContent
      -- accounts: Read
      -- accounts: Update
      -- accounts: Delete
      :<|> "accounts"
           :> SessionCookie
           :> QueryParam' '[Required] "username" Text
           :> Delete '[JSON] NoContent
      -- accounts: List
      :<|> "accounts"
           :> SessionCookie
           :> Get '[JSON] [T.User]

      -- trips: Create
      :<|> "trips"
           :> SessionCookie
           :> ReqBody '[JSON] T.Trip
           :> Post '[JSON] NoContent
      -- trips: Read
      -- trips: Update
      -- trips: Delete
      :<|> "trips"
           :> SessionCookie
           :> ReqBody '[JSON] T.TripPK
           :> Delete '[JSON] NoContent
      -- trips: List
      :<|> "trips"
           :> Get '[JSON] [T.Trip]

      -- Miscellaneous
      :<|> "login"
           :> ReqBody '[JSON] T.AccountCredentials
           :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie] NoContent)
      :<|> "logout"
           :> SessionCookie
           :> Get '[JSON] (Headers '[Header "Set-Cookie" SetCookie] NoContent)
