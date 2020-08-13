--------------------------------------------------------------------------------G
module Types where
--------------------------------------------------------------------------------
import RIO
import Data.Aeson
import Network.HTTP.Req
import Web.Internal.HttpApiData (ToHttpApiData(..))
import System.Envy (FromEnv, fromEnv, env)
--------------------------------------------------------------------------------

-- | Read from .envrc
data Env = Env
  { envGoogleClientID :: !Text
  , envServerPort :: !Int
  , envClientPort :: !Int
  , envStripeAPIKey :: !Text
  } deriving (Eq, Show)

instance FromEnv Env where
  fromEnv _ = do
    envGoogleClientID <- env "GOOGLE_CLIENT_ID"
    envStripeAPIKey <- env "STRIPE_API_KEY"
    envServerPort <- env "SERVER_PORT"
    envClientPort <- env "CLIENT_PORT"
    pure Env {..}

-- | Application context: a combination of Env and additional values.
data Context = Context
  { contextGoogleClientID :: !Text
  , contextStripeAPIKey :: !Text
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
  parseJSON = withObject "VerifyGoogleSignInRequest" $ \x -> do
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

data CurrencyCode = USD
  deriving (Eq, Show)

instance ToJSON CurrencyCode where
  toJSON USD = String "usd"

instance FromJSON CurrencyCode where
  parseJSON = withText "CurrencyCode" $ \x ->
    case x of
      "usd" -> pure USD
      _ -> fail "Expected a valid currency code like: \"usd\""

instance ToHttpApiData CurrencyCode where
  toQueryParam USD = "usd"

data PaymentIntent = PaymentIntent
  { paymentIntentAmount :: !Int
  , paymentIntentCurrency :: !CurrencyCode
  } deriving (Eq, Show)

instance ToJSON PaymentIntent where
  toJSON PaymentIntent{..} =
    object [ "amount" .= paymentIntentAmount
           , "currency" .= paymentIntentCurrency
           ]

instance FromJSON PaymentIntent where
  parseJSON = withObject "" $ \x -> do
    paymentIntentAmount <- x .: "amount"
    paymentIntentCurrency <- x .: "currency"
    pure PaymentIntent{..}

instance QueryParam PaymentIntent where
  queryParam = undefined

-- All applications have their secrets... Using the secret type ensures that no
-- sensitive information will get printed to the screen.
newtype Secret = Secret Text deriving (Eq)

instance Show Secret where
  show (Secret _) = "[REDACTED]"

instance ToJSON Secret where
  toJSON (Secret x) = toJSON x

instance FromJSON Secret where
  parseJSON = withText "Secret" $ \x -> pure $ Secret x

data CreatePaymentIntentResponse = CreatePaymentIntentResponse
  { clientSecret :: Secret
  } deriving (Eq, Show)

instance ToJSON CreatePaymentIntentResponse where
  toJSON CreatePaymentIntentResponse{..} =
    object [ "clientSecret" .= clientSecret
           ]

data StripePaymentIntent = StripePaymentIntent
  { pmtIntentClientSecret :: Secret
  } deriving (Eq, Show)

instance FromJSON StripePaymentIntent where
  parseJSON = withObject "StripeCreatePaymentIntentResponse" $ \x -> do
    pmtIntentClientSecret <- x .: "client_secret"
    pure StripePaymentIntent{..}
