{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
--------------------------------------------------------------------------------
module Stripe where
--------------------------------------------------------------------------------
import RIO
import Prelude (print)
import Data.String.Conversions (cs)
import Data.Aeson
import Network.HTTP.Req

import qualified Types as T
--------------------------------------------------------------------------------

endpoint :: Text -> Url 'Https
endpoint slug =
  https "api.stripe.com" /: "v1" /: slug

post :: (FromJSON b) => Text -> Text -> T.PaymentIntent -> IO (JsonResponse b)
post apiKey slug T.PaymentIntent{..} = runReq defaultHttpConfig $ do
  let params = "amount" =: paymentIntentAmount
            <> "currency" =: paymentIntentCurrency
  req POST (endpoint slug) (ReqBodyUrlEnc params) jsonResponse (oAuth2Bearer (cs apiKey))

createPaymentIntent :: T.Context -> T.PaymentIntent -> IO T.Secret
createPaymentIntent T.Context{..} pmtIntent = do
  res <- post contextStripeAPIKey "payment_intents" pmtIntent
  let T.StripePaymentIntent{..} = responseBody res :: T.StripePaymentIntent
  pure pmtIntentClientSecret
