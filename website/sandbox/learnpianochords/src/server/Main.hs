--------------------------------------------------------------------------------
module Main where
--------------------------------------------------------------------------------
import RIO
import Prelude (putStrLn)

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
      , contextClientPort = 8000
      , contextServerPort = 3000
      }

main :: IO ()
main = do
  mContext <- getAppContext
  case mContext of
    Left err -> putStrLn err
    Right ctx -> runRIO ctx App.run
