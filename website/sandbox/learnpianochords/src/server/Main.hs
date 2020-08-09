--------------------------------------------------------------------------------
module Main where
--------------------------------------------------------------------------------
import Servant
import API
import Control.Monad.IO.Class (liftIO)
import GoogleSignIn (EncodedJWT(..), ValidationResult(..))
import Data.String.Conversions (cs)
import Utils

import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.Cors as Cors
import qualified Types as T
import qualified GoogleSignIn
--------------------------------------------------------------------------------

server :: Server API
server = verifyGoogleSignIn
  where
    verifyGoogleSignIn :: T.VerifyGoogleSignInRequest -> Handler NoContent
    verifyGoogleSignIn T.VerifyGoogleSignInRequest{..} = do
    validationResult <- liftIO $ GoogleSignIn.validateJWT False (EncodedJWT idToken)
    case validationResult of
      Valid _ -> do
        liftIO $ putStrLn "Sign-in valid! Let's create a session"
        pure NoContent
      err -> do
        throwError err401 { errBody = err |> GoogleSignIn.explainResult |> cs }

main :: IO ()
main = do
  Warp.run 3000 (enforceCors $ serve (Proxy @ API) $ server)
  where
    enforceCors = Cors.cors (const $ Just corsPolicy)
    corsPolicy :: Cors.CorsResourcePolicy
    corsPolicy =
      Cors.simpleCorsResourcePolicy
        { Cors.corsOrigins = Just (["http://localhost:8000"], True)
        , Cors.corsMethods = Cors.simpleMethods ++ ["PUT", "PATCH", "DELETE", "OPTIONS"]
        , Cors.corsRequestHeaders = Cors.simpleHeaders ++ ["Content-Type", "Authorization"]
        }
