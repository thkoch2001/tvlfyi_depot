{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------------------
module Main where
--------------------------------------------------------------------------------
import Servant
import API
import Control.Monad.IO.Class (liftIO)

import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.Cors as Cors
import qualified Types as T
--------------------------------------------------------------------------------

server :: Server API
server = verifyGoogleSignIn
  where
    verifyGoogleSignIn :: T.VerifyGoogleSignInRequest -> Handler NoContent
    verifyGoogleSignIn T.VerifyGoogleSignInRequest{..} = do
      liftIO $ putStrLn $ "Received: " ++ idToken
      pure NoContent

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
