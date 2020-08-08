--------------------------------------------------------------------------------
module GoogleSignIn where
--------------------------------------------------------------------------------
import Web.JWT
--------------------------------------------------------------------------------

-- | Returns True when the supplied `jwt` meets the following criteria:
-- * The token has been signed by Google
-- * The value of `aud` matches my Google client's ID
-- * The value of `iss` matches is "accounts.google.com" or
--   "https://accounts.google.com"
-- * The `exp` time has not passed
jwtIsValid :: JWT UnverifiedJWT -> Bool
jwtIsValid jwt = False
