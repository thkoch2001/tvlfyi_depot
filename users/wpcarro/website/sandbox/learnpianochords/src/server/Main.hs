--------------------------------------------------------------------------------
module Main where
--------------------------------------------------------------------------------
import RIO
import Prelude (putStr, putStrLn)

import qualified Types as T
import qualified System.Envy as Envy
import qualified App
--------------------------------------------------------------------------------

-- | Attempt to read environment variables from the system and initialize the
-- Context data type for our application.
getAppContext :: IO (Either String T.Context)
getAppContext = do
  mEnv <- Envy.decodeEnv
  case mEnv of
    Left err -> pure $ Left err
    Right T.Env{..} -> pure $ Right T.Context
      { contextGoogleClientID = envGoogleClientID
      , contextStripeAPIKey = envStripeAPIKey
      , contextServerPort = envServerPort
      , contextClientPort = envClientPort
      }

main :: IO ()
main = do
  mContext <- getAppContext
  case mContext of
    Left err -> putStrLn err
    Right ctx -> do
      result <- runRIO ctx App.run
      case result of
        Left err -> do
          putStr "Something went wrong when executing the application: "
          putStrLn $ show err
        Right _ -> putStrLn "The application successfully executed."
