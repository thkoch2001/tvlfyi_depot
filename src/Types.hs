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
import Data.Text
import Database.Persist.TH
--------------------------------------------------------------------------------

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Account
  username Text
  password Text
  email Text
  role Text
  UniqueUsername username
  UniqueEmail email
  deriving Eq Read Show
|]

instance FromJSON Account where
  parseJSON = withObject "Account" $ \ v ->
    Account <$> v .: "username"
            <*> v .: "password"
            <*> v .: "email"
            <*> v .: "role"

instance ToJSON Account where
  toJSON (Account{ accountUsername
                 , accountPassword
                 , accountEmail
                 , accountRole }) =
    object [ "username" .= accountUsername
           , "password" .= accountPassword
           , "email" .= accountEmail
           , "role" .= accountRole
           ]

newtype Username = Username Text
  deriving (Eq, Show)

instance ToJSON Username where
  toJSON (Username x) = toJSON x

newtype Password = Password Text
  deriving (Eq, Show)

instance ToJSON Password where
  toJSON (Password x) = toJSON x

data Role = RegularUser | Manager | Admin
  deriving (Eq, Show)

instance ToJSON Role where
  toJSON RegularUser = "user"
  toJSON Manager = "manager"
  toJSON Admin = "admin"

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
