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
  , envServerPort :: !Int
  , envClientPort :: !Int
  } deriving (Eq, Show)

instance FromEnv Env where
  fromEnv _ = do
    envGoogleClientID <- env "GOOGLE_CLIENT_ID"
    envServerPort <- env "SERVER_PORT"
    envClientPort <- env "CLIENT_PORT"
    pure Env {..}

-- | Application context: a combination of Env and additional values.
data Context = Context
  { contextGoogleClientID :: !String
  , contextServerPort :: !Int
  , contextClientPort :: !Int
  }

-- | Top-level except for our application, as RIO recommends defining.
type Failure = ()

-- | When our app executes along the "happy path" this is the type of result it
-- produces.
type Success = ()

-- | This is our application monad.
type AppM = RIO Context

-- | The concrete type of our application.
type App = AppM (Either Failure Success)

data VerifyGoogleSignInRequest = VerifyGoogleSignInRequest
  { idToken :: !Text
  } deriving (Eq, Show)

instance FromJSON VerifyGoogleSignInRequest where
  parseJSON = withObject "" $ \x -> do
    idToken <- x .: "idToken"
    pure VerifyGoogleSignInRequest{..}

data GoogleLinkedAccount = GoogleLinkedAccount
  {
  -- { googleLinkedAccountUUID :: UUID
  -- , googleLinkedAccountEmail :: Email
  -- , googleLinkedAccountTsCreated :: Timestamp
    googleLinkedAccountGivenName :: !(Maybe Text)
  , googleLinkedAccountFamilyName :: !(Maybe Text)
  , googleLinkedAccountFullName :: !(Maybe Text)
  -- , googleLinkedAccountPictureURL :: URL
  -- , googleLinkedAccountLocale :: Maybe Locale
  } deriving (Eq, Show)

data PayingCustomer = PayingCustomer
  {
  -- { payingCustomerAccountUUID :: UUID
  -- , payingCustomerTsCreated :: Timestamp
  } deriving (Eq, Show)

data Session = Session
  {
  -- { sessionUUID :: UUID
  -- , sessionAccountUUID :: UUID
  -- , sessionTsCreated :: Timestamp
  } deriving (Eq, Show)
