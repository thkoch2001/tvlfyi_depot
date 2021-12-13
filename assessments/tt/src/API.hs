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
type SessionCookie = Header' '[Required] "Cookie" T.SessionCookie

type API =
      -- accounts: Create
           "accounts"
           :> Header "Cookie" T.SessionCookie
           :> ReqBody '[JSON] T.CreateAccountRequest
           :> Post '[JSON] NoContent
      :<|> "verify"
           :> ReqBody '[JSON] T.VerifyAccountRequest
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
      :<|> "trips"
           :> SessionCookie
           :> ReqBody '[JSON] T.UpdateTripRequest
           :> Put '[JSON] NoContent
      -- trips: Delete
      :<|> "trips"
           :> SessionCookie
           :> ReqBody '[JSON] T.TripPK
           :> Delete '[JSON] NoContent
      -- trips: List
      :<|> "trips"
           :> SessionCookie
           :> Get '[JSON] [T.Trip]

      -- Miscellaneous
      :<|> "login"
           :> ReqBody '[JSON] T.AccountCredentials
           :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie] T.Session)
      :<|> "logout"
           :> SessionCookie
           :> Get '[JSON] (Headers '[Header "Set-Cookie" SetCookie] NoContent)
      :<|> "unfreeze"
           :> SessionCookie
           :> ReqBody '[JSON] T.UnfreezeAccountRequest
           :> Post '[JSON] NoContent
      :<|> "invite"
           :> SessionCookie
           :> ReqBody '[JSON] T.InviteUserRequest
           :> Post '[JSON] NoContent
      :<|> "accept-invitation"
           :> ReqBody '[JSON] T.AcceptInvitationRequest
           :> Post '[JSON] NoContent
