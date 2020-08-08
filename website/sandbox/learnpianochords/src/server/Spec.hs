{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
module Spec where
--------------------------------------------------------------------------------
import Test.Hspec
import Web.JWT
import Utils

import qualified GoogleSignIn
import qualified Fixtures as F
--------------------------------------------------------------------------------

main :: IO ()
main = hspec $ do
  describe "GoogleSignIn" $ do
    describe "jwtIsValid" $ do
      it "returns false when the signature is invalid" $ do
        let mJWT = F.defaultJWTFields { F.overwriteSigner = hmacSecret "wrong" }
                   |> F.googleJWT
        case mJWT of
          Nothing  -> True `shouldBe` False
          Just jwt -> GoogleSignIn.jwtIsValid jwt `shouldReturn` False

      it "returns false when the aud field doesn't match my client ID" $ do
        let mJWT = F.defaultJWTFields { F.overwriteAud = stringOrURI "wrong" }
                  |> F.googleJWT
        case mJWT of
          Nothing  -> True `shouldBe` False
          Just jwt -> GoogleSignIn.jwtIsValid jwt `shouldReturn` False
