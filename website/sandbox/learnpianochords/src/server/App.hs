--------------------------------------------------------------------------------
module App where
--------------------------------------------------------------------------------
import RIO hiding (Handler)
import Servant
import API
import Data.String.Conversions (cs)
import Control.Monad.IO.Class (liftIO)
import Network.Wai.Middleware.Cors
import GoogleSignIn (EncodedJWT(..), ValidationResult(..))
import Utils

import qualified Network.Wai.Handler.Warp as Warp
import qualified GoogleSignIn
import qualified Stripe
import qualified Types as T
--------------------------------------------------------------------------------

server :: T.Context -> Server API
server ctx@T.Context{..} = verifyGoogleSignIn
                      :<|> createPaymentIntent
  where
    verifyGoogleSignIn :: T.VerifyGoogleSignInRequest -> Handler NoContent
    verifyGoogleSignIn T.VerifyGoogleSignInRequest{..} = do
      validationResult <- liftIO $ GoogleSignIn.validateJWT False (EncodedJWT idToken)
      case validationResult of
        Valid _ -> do
          -- If GoogleLinkedAccounts has email from JWT:
          --   create a new session for email
          -- Else:
          --   Redirect the SPA to the sign-up / payment page
          pure NoContent
        err -> do
          throwError err401 { errBody = err |> GoogleSignIn.explainResult |> cs }

    createPaymentIntent :: T.PaymentIntent -> Handler T.CreatePaymentIntentResponse
    createPaymentIntent pmt = do
      clientSecret <- liftIO $ Stripe.createPaymentIntent ctx pmt
      pure T.CreatePaymentIntentResponse{..}

run :: T.App
run = do
  ctx@T.Context{..} <- ask
  ctx
    |> server
    |> serve (Proxy @ API)
    |> cors (const $ Just corsPolicy)
    |> Warp.run contextServerPort
    |> liftIO
  pure $ Right ()
  where
    corsPolicy :: CorsResourcePolicy
    corsPolicy = simpleCorsResourcePolicy
      { corsOrigins = Just (["http://localhost:8000"], True)
      , corsMethods = simpleMethods ++ ["PUT", "PATCH", "DELETE", "OPTIONS"]
      , corsRequestHeaders = simpleHeaders ++ ["Content-Type", "Authorization"]
      }
