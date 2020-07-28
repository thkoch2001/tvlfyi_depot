{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NamedFieldPuns #-}
--------------------------------------------------------------------------------
module Types where
--------------------------------------------------------------------------------
import Data.Aeson
import Data.Function ((&))
import Data.Text
import Data.Typeable
import Database.Persist.TH
import Database.SQLite.Simple
import Database.SQLite.Simple.Ok
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import GHC.Generics

import qualified Data.ByteString as BS
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

newtype Password = Password Text
  deriving (Eq, Show, Generic)

instance ToJSON Password
instance FromJSON Password

instance ToField Password where
  toField (Password x) = SQLText x

instance FromField Password where
  fromField = forNewtype Password

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

instance ToJSON Role
instance FromJSON Role

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
  , accountPassword :: Password
  , accountEmail :: Email
  , accountRole :: Role
  , accountProfilePicture :: ProfilePicture
  } deriving (Eq, Show, Generic)

-- TODO(wpcarro): Prefer username to accountUsername for JSON
instance ToJSON Account
instance FromJSON Account

-- | Return a tuple with all of the fields for an Account record to use for SQL.
accountFields :: Account -> (Username, Password, Email, Role, ProfilePicture)
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
  , password :: Password
  , role :: Role
  } deriving (Eq, Show)

instance ToJSON Session where
  toJSON (Session username password role) =
    object [ "username" .= username
           , "password" .= password
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

-- TODO(wpcarro): Prefer username to tripUsername for JSON
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

-- TODO(wpcarro): Prefer shorter JSON fields like username instead of
-- tripPKUsername.
instance FromJSON TripPK

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

instance ToJSON Trip
instance FromJSON Trip
