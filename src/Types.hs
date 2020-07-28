{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Crypto.Random.Types (MonadRandom)

import qualified Crypto.KDF.BCrypt as BC
import qualified Data.Time.Clock as Clock
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TE
import qualified Data.UUID as UUID
--------------------------------------------------------------------------------

-- TODO(wpcarro): Properly handle NULL for columns like profilePicture.
forNewtype :: (Typeable b) => (Text -> b) -> FieldParser b
forNewtype wrapper field =
  case fieldData field of
    (SQLText x) -> Ok (wrapper x)
    _ -> returnError ConversionFailed field ""

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
      _ -> returnError ConversionFailed field ""

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
      _ -> returnError ConversionFailed field ""

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
accountFields (Account { accountUsername
                       , accountPassword
                       , accountEmail
                       , accountRole
                       , accountProfilePicture
                       })
  = ( accountUsername
    , accountPassword
    , accountEmail
    , accountRole
    , accountProfilePicture
    )

instance FromRow Account where
  fromRow = Account <$> field
                    <*> field
                    <*> field
                    <*> field
                    <*> field

data Session = Session
  { username :: Username
  , role :: Role
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
  fromRow = Trip <$> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field

-- | The fields used as the Primary Key for a Trip entry.
data TripPK = TripPK
  { tripPKUsername :: Username
  , tripPKDestination :: Destination
  , tripPKStartDate :: Date
  } deriving (Eq, Show, Generic)

tripPKFields :: TripPK -> (Username, Destination, Date)
tripPKFields (TripPK{ tripPKUsername
                    , tripPKDestination
                    , tripPKStartDate
                    })
  = (tripPKUsername, tripPKDestination, tripPKStartDate)

instance FromJSON TripPK where
  parseJSON = withObject "TripPK" $ \x -> do
    username <- x .: "username"
    destination <- x .: "destination"
    startDate <- x .: "startDate"
    pure TripPK{ tripPKUsername = username
               , tripPKDestination = destination
               , tripPKStartDate = startDate
               }

-- | Return the tuple representation of a Trip record for SQL.
tripFields :: Trip -> (Username, Destination, Date, Date, Comment)
tripFields (Trip{ tripUsername
                , tripDestination
                , tripStartDate
                , tripEndDate
                , tripComment
                })
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
    username <- x .: "username"
    destination <- x .: "destination"
    startDate <- x .: "startDate"
    endDate <- x .: "endDate"
    comment <- x .: "comment"
    pure Trip{ tripUsername = username
             , tripDestination = destination
             , tripStartDate = startDate
             , tripEndDate = endDate
             , tripComment = comment
             }

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
    username <- x.: "username"
    password <- x.: "password"
    pure AccountCredentials{ accountCredentialsUsername = username
                           , accountCredentialsPassword = password
                           }


-- | Hash password `x`.
hashPassword :: (MonadRandom m) => ClearTextPassword -> m HashedPassword
hashPassword (ClearTextPassword x) = do
  hashed <- BC.hashPassword 12 (x |> unpack |> B.pack)
  pure $ HashedPassword hashed

data CreateAccountRequest = CreateAccountRequest
  { createAccountRequestUsername :: Username
  , createAccountRequestPassword :: ClearTextPassword
  , createAccountRequestEmail :: Email
  , createAccountRequestRole :: Role
  } deriving (Eq, Show)

instance FromJSON CreateAccountRequest where
  parseJSON = withObject "CreateAccountRequest" $ \x -> do
    username <- x .: "username"
    password <- x .: "password"
    email <- x .: "email"
    role <- x .: "role"
    pure $ CreateAccountRequest username password email role

createAccountRequestFields :: CreateAccountRequest -> (Username, ClearTextPassword, Email, Role)
createAccountRequestFields request =
  ( createAccountRequestUsername request
  , createAccountRequestPassword request
  , createAccountRequestEmail request
  , createAccountRequestRole request
  )

newtype SessionUUID = SessionUUID UUID.UUID
  deriving (Eq, Show, Generic)

instance FromField SessionUUID where
  fromField field =
    case fieldData field of
      (SQLText x) ->
        case UUID.fromText x of
          Nothing -> returnError ConversionFailed field ""
          Just x -> Ok $ SessionUUID x
      _ -> returnError ConversionFailed field ""

instance ToField SessionUUID where
  toField (SessionUUID uuid) =
    uuid |> UUID.toText |> SQLText

data StoredSession = StoredSession
  { storedSessionUUID :: SessionUUID
  , storedSessionUsername :: Username
  , storedSessionTsCreated :: Clock.UTCTime
  } deriving (Eq, Show, Generic)

instance FromRow StoredSession where
  fromRow = StoredSession <$> field
                          <*> field
                          <*> field
