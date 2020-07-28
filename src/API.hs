{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
--------------------------------------------------------------------------------
module API where
--------------------------------------------------------------------------------
import Data.Text
import Servant.API

import qualified Types as T
--------------------------------------------------------------------------------

type API = "user"
           :> ReqBody '[JSON] T.Account
           :> Post '[JSON] (Maybe T.Session)
      :<|> "user"
           :> Capture "name" Text
           :> Get  '[JSON] (Maybe T.Account)
      :<|> "trip"
           :> ReqBody '[JSON] T.Trip
           :> Post '[JSON] Bool
