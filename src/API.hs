{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
--------------------------------------------------------------------------------
module API where
--------------------------------------------------------------------------------
import Data.Text
import Servant.API

import qualified Types as T
--------------------------------------------------------------------------------

type API =
      -- accounts: Create
           "accounts"
           :> ReqBody '[JSON] T.CreateAccountRequest
           :> Post '[JSON] NoContent
      -- accounts: Read
      -- accounts: Update
      -- accounts: Delete
      :<|> "accounts"
           :> QueryParam' '[Required] "username" Text
           :> Delete '[JSON] NoContent
      -- accounts: List
      :<|> "accounts"
           :> Get '[JSON] [T.User]

      -- trips: Create
      :<|> "trips"
           :> ReqBody '[JSON] T.Trip
           :> Post '[JSON] NoContent
      -- trips: Read
      -- trips: Update
      -- trips: Delete
      :<|> "trips"
           :> ReqBody '[JSON] T.TripPK
           :> Delete '[JSON] NoContent
      -- trips: List
      :<|> "trips"
           :> Get '[JSON] [T.Trip]

      -- Miscellaneous
      :<|> "login"
           :> ReqBody '[JSON] T.AccountCredentials
           :> Post '[JSON] NoContent
