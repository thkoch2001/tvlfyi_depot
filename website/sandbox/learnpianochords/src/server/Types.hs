{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
module Types where
--------------------------------------------------------------------------------
import Data.Aeson
--------------------------------------------------------------------------------

data VerifyGoogleSignInRequest = VerifyGoogleSignInRequest
  { idToken :: String
  } deriving (Eq, Show)

instance FromJSON VerifyGoogleSignInRequest where
  parseJSON = withObject "" $ \x -> do
    idToken <- x .: "idToken"
    pure VerifyGoogleSignInRequest{..}
