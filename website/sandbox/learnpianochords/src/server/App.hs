--------------------------------------------------------------------------------
module App where
--------------------------------------------------------------------------------
import RIO hiding (Handler)
import Servant
import API
import Data.String.Conversions (cs)
import Control.Monad.IO.Class (liftIO)
import GoogleSignIn (EncodedJWT(..), ValidationResult(..))
import Utils

import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.Cors as Cors
import qualified GoogleSignIn
import qualified Types as T
--------------------------------------------------------------------------------

server :: Server API
server = verifyGoogleSignIn
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

run :: RIO T.Context ()
run = do
  T.Context{..} <- ask
  liftIO $ Warp.run contextServerPort (enforceCors $ serve (Proxy @ API) $ server)
  where
    enforceCors = Cors.cors (const $ Just corsPolicy)
    corsPolicy :: Cors.CorsResourcePolicy
    corsPolicy =
      Cors.simpleCorsResourcePolicy
        { Cors.corsOrigins = Just (["http://localhost:8000"], True)
        , Cors.corsMethods = Cors.simpleMethods ++ ["PUT", "PATCH", "DELETE", "OPTIONS"]
        , Cors.corsRequestHeaders = Cors.simpleHeaders ++ ["Content-Type", "Authorization"]
        }
