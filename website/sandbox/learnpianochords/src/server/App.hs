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
import qualified Types as T
--------------------------------------------------------------------------------

server :: ServerT API T.App
server = verifyGoogleSignIn
  where
    verifyGoogleSignIn :: T.VerifyGoogleSignInRequest -> T.App NoContent
    verifyGoogleSignIn T.VerifyGoogleSignInRequest{..} = do
      T.Context{..} <- ask
      validationResult <- liftIO $ GoogleSignIn.validateJWT False (EncodedJWT idToken)
      case validationResult of
        Valid _ -> do
          -- If GoogleLinkedAccounts has email from JWT:
          --   create a new session for email
          -- Else:
          --   Redirect the SPA to the sign-up / payment page
          pure NoContent
        err -> do
          -- TODO: I would prefer to use `throwError` here, but after changing
          -- to ServerT, I couldn't get the code to compile.
          throwIO err401 { errBody = err |> GoogleSignIn.explainResult |> cs }

run :: T.App ()
run = do
  ctx@T.Context{..} <- ask
  server
    |> hoistServer (Proxy @ API) (runRIO ctx)
    |> serve (Proxy @ API)
    |> cors (const $ Just corsPolicy)
    |> Warp.run contextServerPort
    |> liftIO
  where
    corsPolicy :: CorsResourcePolicy
    corsPolicy = simpleCorsResourcePolicy
      { corsOrigins = Just (["http://localhost:8000"], True)
      , corsMethods = simpleMethods ++ ["PUT", "PATCH", "DELETE", "OPTIONS"]
      , corsRequestHeaders = simpleHeaders ++ ["Content-Type", "Authorization"]
      }
