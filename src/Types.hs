{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
--------------------------------------------------------------------------------
module Types where
--------------------------------------------------------------------------------
import Data.Aeson
import Data.Text
import Database.Persist.TH
--------------------------------------------------------------------------------

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
  name Text
  age  Int
  UniqueName name
  deriving Eq Read Show
|]

instance FromJSON User where
  parseJSON = withObject "User" $ \ v ->
    User <$> v .: "name"
         <*> v .: "age"

instance ToJSON User where
  toJSON (User name age) =
    object [ "name" .= name
           , "age"  .= age
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
