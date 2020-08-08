{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------------------
module Fixtures where
--------------------------------------------------------------------------------
import Web.JWT
import Utils

import qualified Data.Map as Map
import qualified GoogleSignIn
import qualified TestUtils
--------------------------------------------------------------------------------

-- | These are the JWT fields that I'd like to overwrite in the `googleJWT`
-- function.
data JWTFields = JWTFields
  { overwriteSigner :: Signer
  , overwriteAuds :: [StringOrURI]
  }

defaultJWTFields :: JWTFields
defaultJWTFields = JWTFields
  { overwriteSigner = hmacSecret "secret"
  , overwriteAuds = ["771151720060-buofllhed98fgt0j22locma05e7rpngl.apps.googleusercontent.com"]
                    |> fmap TestUtils.unsafeStringOrURI
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
      { iss = stringOrURI "accounts.google.com"
      , sub = stringOrURI "114079822315085727057"
      , aud = overwriteAuds |> Right |> Just
      -- TODO: Replace date creation with a human-readable date constructor.
      , Web.JWT.exp = numericDate 1596756453
      , nbf = Nothing
      -- TODO: Replace date creation with a human-readable date constructor.
      , iat = numericDate 1596752853
      , unregisteredClaims = ClaimsMap (Map.fromList [])
      , jti = stringOrURI "0d3d7fa1fe05bedec0a91c88294936b2b4d1b13c"
      }
