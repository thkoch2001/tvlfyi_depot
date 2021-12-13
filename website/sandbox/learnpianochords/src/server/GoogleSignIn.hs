--------------------------------------------------------------------------------
module GoogleSignIn where
--------------------------------------------------------------------------------
import RIO
import Data.String.Conversions (cs)
import Web.JWT
import Utils

import qualified Network.HTTP.Simple as HTTP
import qualified Data.Text as Text
import qualified Web.JWT as JWT
import qualified Data.Time.Clock.POSIX as POSIX
--------------------------------------------------------------------------------

newtype EncodedJWT = EncodedJWT Text
  deriving (Show)

newtype DecodedJWT = DecodedJWT (JWT UnverifiedJWT)
  deriving (Show)

instance Eq DecodedJWT where
  (DecodedJWT _) == (DecodedJWT _) = True

data ValidationResult
  = Valid DecodedJWT
  | CannotDecodeJWT
  | GoogleSaysInvalid Text
  | NoMatchingClientIDs [StringOrURI]
  | WrongIssuer StringOrURI
  | StringOrURIParseFailure Text
  | TimeConversionFailure
  | MissingRequiredClaim Text
  | StaleExpiry NumericDate
  deriving (Eq, Show)

-- | Returns True when the supplied `jwt` meets the following criteria:
-- * The token has been signed by Google
-- * The value of `aud` matches my Google client's ID
-- * The value of `iss` matches is "accounts.google.com" or
--   "https://accounts.google.com"
-- * The `exp` time has not passed
--
-- Set `skipHTTP` to `True` to avoid making the network request for testing.
validateJWT :: Bool
           -> EncodedJWT
           -> IO ValidationResult
validateJWT skipHTTP (EncodedJWT encodedJWT) = do
  case encodedJWT |> decode of
    Nothing -> pure CannotDecodeJWT
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
      let audValues :: [StringOrURI]
          audValues = jwt |> claims |> auds
          expectedClientID :: Text
          expectedClientID = "771151720060-buofllhed98fgt0j22locma05e7rpngl.apps.googleusercontent.com"
          expectedIssuers :: [Text]
          expectedIssuers = [ "accounts.google.com"
                            , "https://accounts.google.com"
                            ]
          mExpectedClientID :: Maybe StringOrURI
          mExpectedClientID = stringOrURI expectedClientID
          mExpectedIssuers :: Maybe [StringOrURI]
          mExpectedIssuers = expectedIssuers |> traverse stringOrURI
      case (mExpectedClientID, mExpectedIssuers) of
        (Nothing, _) -> pure $ StringOrURIParseFailure expectedClientID
        (_, Nothing) -> pure $ StringOrURIParseFailure (Text.unwords expectedIssuers)
        (Just clientID, Just parsedIssuers) ->
          -- TODO: Prefer reading clientID from a config. I'm thinking of the
          -- AppContext type having my Configuration
          if not $ clientID `elem` audValues then
            pure $ NoMatchingClientIDs audValues
          else
            case (jwt |> claims |> iss, jwt |> claims |> JWT.exp) of
              (Nothing, _) -> pure $ MissingRequiredClaim "iss"
              (_, Nothing) -> pure $ MissingRequiredClaim "exp"
              (Just jwtIssuer, Just jwtExpiry) ->
                if not $ jwtIssuer `elem` parsedIssuers then
                  pure $ WrongIssuer jwtIssuer
                else do
                  mCurrentTime <- POSIX.getPOSIXTime |> fmap numericDate
                  case mCurrentTime of
                    Nothing -> pure TimeConversionFailure
                    Just currentTime ->
                      if not $ currentTime <= jwtExpiry then
                        pure $ StaleExpiry jwtExpiry
                      else
                        pure $ jwt |> DecodedJWT |> Valid

-- | Attempt to explain the `ValidationResult` to a human.
explainResult :: ValidationResult -> String
explainResult (Valid _) = "Everything appears to be valid"
explainResult CannotDecodeJWT = "We had difficulty decoding the provided JWT"
explainResult (GoogleSaysInvalid x) = "After checking with Google, they claimed that the provided JWT was invalid: " ++ cs x
explainResult (NoMatchingClientIDs audFields) = "None of the values in the `aud` field on the provided JWT match our client ID: " ++ show audFields
explainResult (WrongIssuer issuer) = "The `iss` field in the provided JWT does not match what we expect: " ++ show issuer
explainResult (StringOrURIParseFailure x) = "We had difficulty parsing values as URIs" ++ show x
explainResult TimeConversionFailure = "We had difficulty converting the current time to a value we can use to compare with the JWT's `exp` field"
explainResult (MissingRequiredClaim claim) = "Your JWT is missing the following claim: " ++ cs claim
explainResult (StaleExpiry x) = "The `exp` field on your JWT has expired" ++ x |> show |> cs
