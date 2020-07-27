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

instance FromJSON Account
instance ToJSON Account

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
