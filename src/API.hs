{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
--------------------------------------------------------------------------------
module API where
--------------------------------------------------------------------------------
import Data.Proxy
import Data.Text
import Database.Persist
import Servant.API

import qualified Types as T
--------------------------------------------------------------------------------

type API = "user"
           :> ReqBody '[JSON] T.Account
           :> Post '[JSON] (Maybe T.Session)
      :<|> "user"
           :> Capture "name" Text
           :> Get  '[JSON] (Maybe T.Account)
