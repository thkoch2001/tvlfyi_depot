{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
--------------------------------------------------------------------------------
module API where
--------------------------------------------------------------------------------
import Servant.API

import qualified Types as T
--------------------------------------------------------------------------------

type API = "verify"
           :> ReqBody '[JSON] T.VerifyGoogleSignInRequest
           :> Post '[JSON] NoContent
      :<|> "create-payment-intent"
           :> ReqBody '[JSON] T.PaymentIntent
           :> Post '[JSON] T.CreatePaymentIntentResponse
