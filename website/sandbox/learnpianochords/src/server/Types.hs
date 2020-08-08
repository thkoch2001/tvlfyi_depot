{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
module Types where
--------------------------------------------------------------------------------
import Data.Aeson
import Data.Text
--------------------------------------------------------------------------------

data VerifyGoogleSignInRequest = VerifyGoogleSignInRequest
  { idToken :: Text
  } deriving (Eq, Show)

instance FromJSON VerifyGoogleSignInRequest where
  parseJSON = withObject "" $ \x -> do
    idToken <- x .: "idToken"
    pure VerifyGoogleSignInRequest{..}
