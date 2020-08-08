{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
module Spec where
--------------------------------------------------------------------------------
import Test.Hspec
import Utils
import GoogleSignIn (ValidationResult(..))

import qualified GoogleSignIn
import qualified Fixtures as F
import qualified TestUtils
--------------------------------------------------------------------------------

main :: IO ()
main = hspec $ do
  describe "GoogleSignIn" $
    describe "jwtIsValid" $ do
      let jwtIsValid' = GoogleSignIn.jwtIsValid True
      it "returns a decode error when an incorrectly encoded JWT is used" $ do
        jwtIsValid' (GoogleSignIn.EncodedJWT "rubbish") `shouldReturn` DecodeError

      it "returns validation error when the aud field doesn't match my client ID" $ do
        let auds = ["wrong-client-id"]
                   |> fmap TestUtils.unsafeStringOrURI
            encodedJWT = F.defaultJWTFields { F.overwriteAuds = auds }
                         |> F.googleJWT
        jwtIsValid' encodedJWT `shouldReturn` NoMatchingClientIDs auds

      it "returns validation success when one of the aud fields matches my client ID" $ do
        let auds = ["wrong-client-id", "771151720060-buofllhed98fgt0j22locma05e7rpngl.apps.googleusercontent.com"]
                   |> fmap TestUtils.unsafeStringOrURI
            encodedJWT = F.defaultJWTFields { F.overwriteAuds = auds }
                         |> F.googleJWT
        jwtIsValid' encodedJWT `shouldReturn` Valid
