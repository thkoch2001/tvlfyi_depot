{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
--------------------------------------------------------------------------------
module Types where
--------------------------------------------------------------------------------
import Data.Aeson
import Utils
import Data.Text
import Data.Typeable
import Database.SQLite.Simple
import Database.SQLite.Simple.Ok
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import GHC.Generics
import Web.Cookie
import Servant.API
import System.Envy (FromEnv, fromEnv, env)
import Crypto.Random.Types (MonadRandom)

import qualified Crypto.KDF.BCrypt as BC
import qualified Data.Time.Clock as Clock
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TE
import qualified Data.Maybe as M
import qualified Data.UUID as UUID
--------------------------------------------------------------------------------

-- | Top-level application configuration.
data Config = Config
  { mailgunAPIKey :: Text
  , dbFile :: FilePath
  , configClient :: Text
  , configServer :: Text
  } deriving (Eq, Show)

instance FromEnv Config where
  fromEnv _ = do
    mailgunAPIKey <- env "MAILGUN_API_KEY"
    dbFile <- env "DB_FILE"
    configClient <- env "CLIENT"
    configServer <- env "SERVER"
    pure Config {..}

-- TODO(wpcarro): Properly handle NULL for columns like profilePicture.
forNewtype :: (Typeable b) => (Text -> b) -> FieldParser b
forNewtype wrapper field =
  case fieldData field of
    (SQLText x) -> Ok (wrapper x)
    x -> returnError ConversionFailed field ("We expected SQLText, but we received: " ++ show x)

newtype Username = Username Text
  deriving (Eq, Show, Generic)

instance ToJSON Username
instance FromJSON Username

instance ToField Username where
  toField (Username x) = SQLText x

instance FromField Username where
  fromField = forNewtype Username

newtype HashedPassword = HashedPassword BS.ByteString
  deriving (Eq, Show, Generic)

instance ToField HashedPassword where
  toField (HashedPassword x) = SQLText (TE.decodeUtf8 x)

instance FromField HashedPassword where
  fromField field =
    case fieldData field of
      (SQLText x) -> x |> TE.encodeUtf8 |> HashedPassword |> Ok
      x -> returnError ConversionFailed field ("We expected SQLText, but we received: " ++ show x)

newtype ClearTextPassword = ClearTextPassword Text
  deriving (Eq, Show, Generic)

instance ToJSON ClearTextPassword
instance FromJSON ClearTextPassword

instance ToField ClearTextPassword where
  toField (ClearTextPassword x) = SQLText x

instance FromField ClearTextPassword where
  fromField = forNewtype ClearTextPassword

newtype Email = Email Text
  deriving (Eq, Show, Generic)

instance ToJSON Email
instance FromJSON Email

instance ToField Email where
  toField (Email x) = SQLText x

instance FromField Email where
  fromField = forNewtype Email

data Role = RegularUser | Manager | Admin
  deriving (Eq, Show, Generic)

instance ToJSON Role where
  toJSON RegularUser = "user"
  toJSON Manager = "manager"
  toJSON Admin = "admin"

instance FromJSON Role where
  parseJSON = withText "Role" $ \x ->
    case x of
      "user" -> pure RegularUser
      "manager" -> pure Manager
      "admin" -> pure Admin
      _ -> fail "Expected \"user\" or \"manager\" or \"admin\""

instance ToField Role where
  toField RegularUser = SQLText "user"
  toField Manager = SQLText "manager"
  toField Admin = SQLText "admin"

instance FromField Role where
  fromField field =
    case fieldData field of
      (SQLText "user") -> Ok RegularUser
      (SQLText "manager") -> Ok Manager
      (SQLText "admin") -> Ok Admin
      x -> returnError ConversionFailed field ("We expected user, manager, admin, but we received: " ++ show x)

-- TODO(wpcarro): Prefer Data.ByteString instead of Text
newtype ProfilePicture = ProfilePicture Text
  deriving (Eq, Show, Generic)

instance ToJSON ProfilePicture
instance FromJSON ProfilePicture

instance ToField ProfilePicture where
  toField (ProfilePicture x) = SQLText x

instance FromField ProfilePicture where
  fromField = forNewtype ProfilePicture

data Account = Account
  { accountUsername :: Username
  , accountPassword :: HashedPassword
  , accountEmail :: Email
  , accountRole :: Role
  , accountProfilePicture :: Maybe ProfilePicture
  } deriving (Eq, Show, Generic)

-- | Return a tuple with all of the fields for an Account record to use for SQL.
accountFields :: Account -> (Username, HashedPassword, Email, Role, Maybe ProfilePicture)
accountFields (Account {..})
  = ( accountUsername
    , accountPassword
    , accountEmail
    , accountRole
    , accountProfilePicture
    )

instance FromRow Account where
  fromRow = do
    accountUsername <- field
    accountPassword <- field
    accountEmail <- field
    accountRole <- field
    accountProfilePicture <- field
    pure Account{..}

data Session = Session
  { sessionUsername :: Username
  , sessionRole :: Role
  } deriving (Eq, Show)

instance ToJSON Session where
  toJSON (Session username role) =
    object [ "username" .= username
           , "role" .= role
           ]

newtype Comment = Comment Text
  deriving (Eq, Show, Generic)

instance ToJSON Comment
instance FromJSON Comment

instance ToField Comment where
  toField (Comment x) = SQLText x

instance FromField Comment where
  fromField = forNewtype Comment

-- TODO(wpcarro): Replace this with a different type.
newtype Date = Date Text
  deriving (Eq, Show, Generic)

instance ToJSON Date
instance FromJSON Date

instance ToField Date where
  toField (Date x) = SQLText x

instance FromField Date where
  fromField = forNewtype Date

newtype Destination = Destination Text
  deriving (Eq, Show, Generic)

instance ToJSON Destination
instance FromJSON Destination

instance ToField Destination where
  toField (Destination x) = SQLText x

instance FromField Destination where
  fromField = forNewtype Destination

data Trip = Trip
  { tripUsername :: Username
  , tripDestination :: Destination
  , tripStartDate :: Date
  , tripEndDate :: Date
  , tripComment :: Comment
  } deriving (Eq, Show, Generic)

instance FromRow Trip where
  fromRow = do
    tripUsername <- field
    tripDestination <- field
    tripStartDate <- field
    tripEndDate <- field
    tripComment <- field
    pure Trip{..}

-- | The fields used as the Primary Key for a Trip entry.
data TripPK = TripPK
  { tripPKUsername :: Username
  , tripPKDestination :: Destination
  , tripPKStartDate :: Date
  } deriving (Eq, Show, Generic)

tripPKFields :: TripPK -> (Username, Destination, Date)
tripPKFields (TripPK{..})
  = (tripPKUsername, tripPKDestination, tripPKStartDate)

instance FromJSON TripPK where
  parseJSON = withObject "TripPK" $ \x -> do
    tripPKUsername    <- x .: "username"
    tripPKDestination <- x .: "destination"
    tripPKStartDate   <- x .: "startDate"
    pure TripPK{..}

-- | Return the tuple representation of a Trip record for SQL.
tripFields :: Trip -> (Username, Destination, Date, Date, Comment)
tripFields (Trip{..})
  = ( tripUsername
    , tripDestination
    , tripStartDate
    , tripEndDate
    , tripComment
    )

instance ToJSON Trip where
  toJSON (Trip username destination startDate endDate comment) =
    object [ "username" .= username
           , "destination" .= destination
           , "startDate" .= startDate
           , "endDate" .= endDate
           , "comment" .= comment
           ]

instance FromJSON Trip where
  parseJSON = withObject "Trip" $ \x -> do
    tripUsername    <- x .: "username"
    tripDestination <- x .: "destination"
    tripStartDate   <- x .: "startDate"
    tripEndDate     <- x .: "endDate"
    tripComment     <- x .: "comment"
    pure Trip{..}

-- | Users and Accounts both refer to the same underlying entities; however,
-- Users model the user-facing Account details, hiding sensitive details like
-- passwords and emails.
data User = User
  { userUsername :: Username
  , userProfilePicture :: Maybe ProfilePicture
  , userRole :: Role
  } deriving (Eq, Show, Generic)

instance ToJSON User where
  toJSON (User username profilePicture role) =
    object [ "username" .= username
           , "profilePicture" .= profilePicture
           , "role" .= role
           ]

userFromAccount :: Account -> User
userFromAccount account =
  User { userUsername = accountUsername account
       , userProfilePicture = accountProfilePicture account
       , userRole = accountRole account
       }

-- | This is the data that a user needs to supply to authenticate with the
-- application.
data AccountCredentials = AccountCredentials
  { accountCredentialsUsername :: Username
  , accountCredentialsPassword :: ClearTextPassword
  } deriving (Eq, Show, Generic)

instance FromJSON AccountCredentials where
  parseJSON = withObject "AccountCredentials" $ \x -> do
    accountCredentialsUsername <- x.: "username"
    accountCredentialsPassword <- x.: "password"
    pure AccountCredentials{..}


-- | Hash password `x`.
hashPassword :: (MonadRandom m) => ClearTextPassword -> m HashedPassword
hashPassword (ClearTextPassword x) = do
  hashed <- BC.hashPassword 12 (x |> unpack |> B.pack)
  pure $ HashedPassword hashed

-- | Return True if the cleartext password matches the hashed password.
passwordsMatch :: ClearTextPassword -> HashedPassword -> Bool
passwordsMatch (ClearTextPassword clear) (HashedPassword hashed) =
  BC.validatePassword (clear |> unpack |> B.pack) hashed

data CreateAccountRequest = CreateAccountRequest
  { createAccountRequestUsername :: Username
  , createAccountRequestPassword :: ClearTextPassword
  , createAccountRequestEmail :: Email
  , createAccountRequestRole :: Role
  } deriving (Eq, Show)

instance FromJSON CreateAccountRequest where
  parseJSON = withObject "CreateAccountRequest" $ \x -> do
    createAccountRequestUsername <- x .: "username"
    createAccountRequestPassword <- x .: "password"
    createAccountRequestEmail <- x .: "email"
    createAccountRequestRole <- x .: "role"
    pure $ CreateAccountRequest{..}

createAccountRequestFields :: CreateAccountRequest
                           -> (Username, ClearTextPassword, Email, Role)
createAccountRequestFields CreateAccountRequest{..} =
  ( createAccountRequestUsername
  , createAccountRequestPassword
  , createAccountRequestEmail
  , createAccountRequestRole
  )

newtype SessionUUID = SessionUUID UUID.UUID
  deriving (Eq, Show, Generic)

instance FromField SessionUUID where
  fromField field =
    case fieldData field of
      (SQLText x) ->
        case UUID.fromText x of
          Nothing -> returnError ConversionFailed field ("Could not convert to UUID: " ++ show x)
          Just x -> Ok $ SessionUUID x
      _ -> returnError ConversionFailed field "Expected SQLText for SessionUUID, but we received"

instance ToField SessionUUID where
  toField (SessionUUID uuid) =
    uuid |> UUID.toText |> SQLText

data StoredSession = StoredSession
  { storedSessionUUID :: SessionUUID
  , storedSessionUsername :: Username
  , storedSessionTsCreated :: Clock.UTCTime
  } deriving (Eq, Show, Generic)

instance FromRow StoredSession where
  fromRow = do
    storedSessionUUID <- field
    storedSessionUsername <- field
    storedSessionTsCreated <- field
    pure StoredSession {..}

data LoginAttempt = LoginAttempt
  { loginAttemptUsername :: Username
  , loginAttemptNumAttempts :: Integer
  } deriving (Eq, Show)

instance FromRow LoginAttempt where
  fromRow = do
    loginAttemptUsername <- field
    loginAttemptNumAttempts <- field
    pure LoginAttempt {..}

newtype SessionCookie = SessionCookie Cookies

instance FromHttpApiData SessionCookie where
  parseHeader x =
    x |> parseCookies |> SessionCookie |> pure
  parseQueryParam x =
    x |> TE.encodeUtf8 |> parseCookies |> SessionCookie |> pure

newtype RegistrationSecret = RegistrationSecret UUID.UUID
  deriving (Eq, Show)

instance FromField RegistrationSecret where
  fromField field =
    case fieldData field of
      (SQLText x) ->
        case UUID.fromText x of
          Nothing -> returnError ConversionFailed field ("Could not convert text to UUID: " ++ show x)
          Just x -> Ok $ RegistrationSecret x
      _ -> returnError ConversionFailed field "Field data is not SQLText, which is what we expect"

instance ToField RegistrationSecret where
  toField (RegistrationSecret secretUUID) =
    secretUUID |> UUID.toText |> SQLText

data PendingAccount = PendingAccount
  { pendingAccountSecret :: RegistrationSecret
  , pendingAccountUsername :: Username
  , pendingAccountPassword :: HashedPassword
  , pendingAccountRole :: Role
  , pendingAccountEmail :: Email
  } deriving (Eq, Show)

instance FromRow PendingAccount where
  fromRow = do
    pendingAccountSecret <- field
    pendingAccountUsername <- field
    pendingAccountPassword <- field
    pendingAccountRole <- field
    pendingAccountEmail <- field
    pure PendingAccount {..}

data UpdateTripRequest = UpdateTripRequest
  { updateTripRequestTripPK :: TripPK
  , updateTripRequestDestination :: Maybe Destination
  , updateTripRequestStartDate :: Maybe Date
  , updateTripRequestEndDate :: Maybe Date
  , updateTripRequestComment :: Maybe Comment
  } deriving (Eq, Show)

instance FromJSON UpdateTripRequest where
  parseJSON = withObject "UpdateTripRequest" $ \x -> do
    updateTripRequestTripPK <- x .: "tripKey"
    -- the following four fields might not be present
    updateTripRequestDestination <- x .:? "destination"
    updateTripRequestStartDate   <- x .:? "startDate"
    updateTripRequestEndDate     <- x .:? "endDate"
    updateTripRequestComment     <- x .:? "comment"
    pure UpdateTripRequest{..}

-- | Apply the updates in the UpdateTripRequest to Trip.
updateTrip :: UpdateTripRequest -> Trip -> Trip
updateTrip UpdateTripRequest{..} Trip{..} = Trip
  { tripUsername    = tripUsername
  , tripDestination = M.fromMaybe tripDestination updateTripRequestDestination
  , tripStartDate   = M.fromMaybe tripStartDate updateTripRequestStartDate
  , tripEndDate     = M.fromMaybe tripEndDate updateTripRequestEndDate
  , tripComment     = M.fromMaybe tripComment updateTripRequestComment
  }

data UnfreezeAccountRequest = UnfreezeAccountRequest
  { unfreezeAccountRequestUsername :: Username
  } deriving (Eq, Show)

instance FromJSON UnfreezeAccountRequest where
  parseJSON = withObject "UnfreezeAccountRequest" $ \x -> do
    unfreezeAccountRequestUsername <- x .: "username"
    pure UnfreezeAccountRequest{..}
