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

      it "returns validation error when one of the iss field doesn't match accounts.google.com or https://accounts.google.com" $ do
        let erroneousIssuer = TestUtils.unsafeStringOrURI "not-accounts.google.com"
            encodedJWT = F.defaultJWTFields { F.overwriteIss = erroneousIssuer }
                         |> F.googleJWT
        jwtIsValid' encodedJWT `shouldReturn` WrongIssuer erroneousIssuer

      it "returns validation success when the iss field matches accounts.google.com or https://accounts.google.com" $ do
        let erroneousIssuer = TestUtils.unsafeStringOrURI "https://accounts.google.com"
            encodedJWT = F.defaultJWTFields { F.overwriteIss = erroneousIssuer }
                         |> F.googleJWT
        jwtIsValid' encodedJWT `shouldReturn` Valid
