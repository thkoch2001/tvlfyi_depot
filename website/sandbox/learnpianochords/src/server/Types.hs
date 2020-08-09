--------------------------------------------------------------------------------
module Types where
--------------------------------------------------------------------------------
import RIO
import Data.Aeson
import System.Envy (FromEnv, fromEnv, env)
--------------------------------------------------------------------------------

-- | Read from .envrc
data Env = Env
  { envGoogleClientID :: !String
  } deriving (Eq, Show)

instance FromEnv Env where
  fromEnv _ = do
    envGoogleClientID <- env "GOOGLE_CLIENT_ID"
    pure Env {..}

-- | Application context: a combination of Env and additional values.
data Context = Context
  { contextGoogleClientID :: !String
  , contextServerPort :: !Int
  , contextClientPort :: !Int
  }

-- | Type synonym for my application monad.
type App = RIO Context

data VerifyGoogleSignInRequest = VerifyGoogleSignInRequest
  { idToken :: !Text
  } deriving (Eq, Show)

instance FromJSON VerifyGoogleSignInRequest where
  parseJSON = withObject "" $ \x -> do
    idToken <- x .: "idToken"
    pure VerifyGoogleSignInRequest{..}
