{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
module GoogleSignIn where
--------------------------------------------------------------------------------
import Data.String.Conversions (cs)
import Data.Text (Text)
import Web.JWT
import Utils

import qualified Network.HTTP.Simple as HTTP
--------------------------------------------------------------------------------

newtype EncodedJWT = EncodedJWT Text

-- | Some of the errors that a JWT
data ValidationResult
  = Valid
  | DecodeError
  | GoogleSaysInvalid Text
  | NoMatchingClientIDs [StringOrURI]
  | ClientIDParseFailure Text
  deriving (Eq, Show)

-- | Returns True when the supplied `jwt` meets the following criteria:
-- * The token has been signed by Google
-- * The value of `aud` matches my Google client's ID
-- * The value of `iss` matches is "accounts.google.com" or
--   "https://accounts.google.com"
-- * The `exp` time has not passed
--
-- Set `skipHTTP` to `True` to avoid making the network request for testing.
jwtIsValid :: Bool
           -> EncodedJWT
           -> IO ValidationResult
jwtIsValid skipHTTP (EncodedJWT encodedJWT) = do
  case encodedJWT |> decode of
    Nothing -> pure DecodeError
    Just jwt -> do
      if skipHTTP then
        continue jwt
      else do
        let request = "https://oauth2.googleapis.com/tokeninfo"
                      |> HTTP.setRequestQueryString [ ( "id_token", Just (cs encodedJWT) ) ]
        res <- HTTP.httpLBS request
        if HTTP.getResponseStatusCode res /= 200 then
          pure $ GoogleSaysInvalid (res |> HTTP.getResponseBody |> cs)
        else
          continue jwt
  where
    continue :: JWT UnverifiedJWT -> IO ValidationResult
    continue jwt = do
      let audValues = jwt |> claims |> auds
          mClientID = stringOrURI "771151720060-buofllhed98fgt0j22locma05e7rpngl.apps.googleusercontent.com"
      case mClientID of
        Nothing ->
          pure $ ClientIDParseFailure "771151720060-buofllhed98fgt0j22locma05e7rpngl.apps.googleusercontent.com"
        Just clientID ->
          -- TODO: Prefer reading clientID from a config. I'm thinking of the
          -- AppContext type having my Configuration
          if not $ clientID `elem` audValues then
            pure $ NoMatchingClientIDs audValues
          else
            pure Valid
