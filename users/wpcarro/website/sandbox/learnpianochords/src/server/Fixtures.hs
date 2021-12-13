--------------------------------------------------------------------------------
module Fixtures where
--------------------------------------------------------------------------------
import RIO
import Web.JWT
import Utils

import qualified Data.Map as Map
import qualified GoogleSignIn
import qualified TestUtils
import qualified Data.Time.Clock.POSIX as POSIX
import qualified System.IO.Unsafe as Unsafe
--------------------------------------------------------------------------------

-- | These are the JWT fields that I'd like to overwrite in the `googleJWT`
-- function.
data JWTFields = JWTFields
  { overwriteSigner :: Signer
  , overwriteAuds :: [StringOrURI]
  , overwriteIss :: StringOrURI
  , overwriteExp :: NumericDate
  }

defaultJWTFields :: JWTFields
defaultJWTFields = do
  let tenDaysFromToday = POSIX.getPOSIXTime
                         |> Unsafe.unsafePerformIO
                         |> (\x -> x * 60 * 60 * 25 * 10)
                         |> numericDate
                         |> TestUtils.unsafeJust
  JWTFields
    { overwriteSigner = hmacSecret "secret"
    , overwriteAuds = ["771151720060-buofllhed98fgt0j22locma05e7rpngl.apps.googleusercontent.com"]
                      |> fmap TestUtils.unsafeStringOrURI
    , overwriteIss = TestUtils.unsafeStringOrURI "accounts.google.com"
    , overwriteExp = tenDaysFromToday
    }

googleJWT :: JWTFields -> GoogleSignIn.EncodedJWT
googleJWT JWTFields{..} =
  encodeSigned signer jwtHeader claimSet
  |> GoogleSignIn.EncodedJWT
  where
    signer :: Signer
    signer = overwriteSigner

    jwtHeader :: JOSEHeader
    jwtHeader = JOSEHeader
      { typ = Just "JWT"
      , cty = Nothing
      , alg = Just RS256
      , kid = Just "f05415b13acb9590f70df862765c655f5a7a019e"
      }

    claimSet :: JWTClaimsSet
    claimSet = JWTClaimsSet
      { iss = Just overwriteIss
      , sub = stringOrURI "114079822315085727057"
      , aud = overwriteAuds |> Right |> Just
      -- TODO: Replace date creation with a human-readable date constructor.
      , Web.JWT.exp = Just overwriteExp
      , nbf = Nothing
      -- TODO: Replace date creation with a human-readable date constructor.
      , iat = numericDate 1596752853
      , unregisteredClaims = ClaimsMap (Map.fromList [])
      , jti = stringOrURI "0d3d7fa1fe05bedec0a91c88294936b2b4d1b13c"
      }
