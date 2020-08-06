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

type API = "verify"
           :> ReqBody '[JSON] T.VerifyGoogleSignInRequest
           :> Post '[JSON] NoContent
