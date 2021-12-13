{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
--------------------------------------------------------------------------------
module App where
--------------------------------------------------------------------------------
import Control.Monad.IO.Class (liftIO)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Servant
import API
import Utils
import Web.Cookie

import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.Cors as Cors
import qualified System.Random as Random
import qualified Email as Email
import qualified Data.UUID as UUID
import qualified Types as T
import qualified Accounts as Accounts
import qualified Auth as Auth
import qualified Trips as Trips
import qualified Sessions as Sessions
import qualified Invitations as Invitations
import qualified LoginAttempts as LoginAttempts
import qualified PendingAccounts as PendingAccounts
--------------------------------------------------------------------------------

err429 :: ServerError
err429 = ServerError
  { errHTTPCode = 429
  , errReasonPhrase = "Too many requests"
  , errBody = ""
  , errHeaders = []
  }

-- | Send an email to recipient, `to`, with a secret code.
sendVerifyEmail :: T.Config
                -> T.Username
                -> T.Email
                -> T.RegistrationSecret
                -> IO (Either Email.SendError Email.SendSuccess)
sendVerifyEmail T.Config{..} (T.Username username) email (T.RegistrationSecret secretUUID) = do
  Email.send mailgunAPIKey subject (cs body) email
  where
    subject = "Please confirm your account"
    body =
      let secret = secretUUID |> UUID.toString in
        "To verify your account: POST /verify username=" ++ cs username ++ " secret=" ++ secret

-- | Send an invitation email to recipient, `to`, with a secret code.
sendInviteEmail :: T.Config
                -> T.Email
                -> T.InvitationSecret
                -> IO (Either Email.SendError Email.SendSuccess)
sendInviteEmail T.Config{..} email@(T.Email to) (T.InvitationSecret secretUUID) = do
  Email.send mailgunAPIKey subject (cs body) email
  where
    subject = "You've been invited!"
    body =
      let secret = secretUUID |> UUID.toString in
        "To accept the invitation: POST /accept-invitation username=<username> password=<password> email=" ++ cs to ++ " secret=" ++ secret

server :: T.Config -> Server API
server config@T.Config{..} = createAccount
                        :<|> verifyAccount
                        :<|> deleteAccount
                        :<|> listAccounts
                        :<|> createTrip
                        :<|> updateTrip
                        :<|> deleteTrip
                        :<|> listTrips
                        :<|> login
                        :<|> logout
                        :<|> unfreezeAccount
                        :<|> inviteUser
                        :<|> acceptInvitation
  where
    -- Admit Admins + whatever the predicate `p` passes.
    adminsAnd cookie p = Auth.assert dbFile cookie (\acct@T.Account{..} -> accountRole == T.Admin || p acct)
    -- Admit Admins only.
    adminsOnly cookie = adminsAnd cookie (const True)

    -- TODO(wpcarro): Handle failed CONSTRAINTs instead of sending 500s
    createAccount :: Maybe T.SessionCookie
                  -> T.CreateAccountRequest
                  -> Handler NoContent
    createAccount mCookie T.CreateAccountRequest{..} =
      case (mCookie, createAccountRequestRole) of
        (_, T.RegularUser) ->
          doCreateAccount
        (Nothing, T.Manager) ->
          throwError err401 { errBody = "Only admins can create Manager accounts" }
        (Nothing, T.Admin) ->
          throwError err401 { errBody = "Only admins can create Admin accounts" }
        (Just cookie, _) ->
          adminsAnd cookie (\T.Account{..} -> accountRole == T.Manager) doCreateAccount
      where
        doCreateAccount :: Handler NoContent
        doCreateAccount = do
          secretUUID <- liftIO $ T.RegistrationSecret <$> Random.randomIO
          liftIO $ PendingAccounts.create dbFile
            secretUUID
            createAccountRequestUsername
            createAccountRequestPassword
            createAccountRequestRole
            createAccountRequestEmail
          res <- liftIO $ sendVerifyEmail config
            createAccountRequestUsername
            createAccountRequestEmail
            secretUUID
          case res of
            Left _ -> undefined
            Right _ -> pure NoContent

    verifyAccount :: T.VerifyAccountRequest -> Handler NoContent
    verifyAccount T.VerifyAccountRequest{..} = do
      mPendingAccount <- liftIO $ PendingAccounts.get dbFile verifyAccountRequestUsername
      case mPendingAccount of
        Nothing ->
          throwError err401 { errBody = "Either your secret or your username (or both) is invalid" }
        Just pendingAccount@T.PendingAccount{..} ->
          if pendingAccountSecret == verifyAccountRequestSecret then do
            liftIO $ Accounts.transferFromPending dbFile pendingAccount
            pure NoContent
          else
            throwError err401 { errBody = "The secret you provided is invalid" }

    deleteAccount :: T.SessionCookie -> Text -> Handler NoContent
    deleteAccount cookie username = adminsOnly cookie $ do
      liftIO $ Accounts.delete dbFile (T.Username username)
      pure NoContent

    listAccounts :: T.SessionCookie -> Handler [T.User]
    listAccounts cookie = adminsOnly cookie $ do
      liftIO $ Accounts.list dbFile

    createTrip :: T.SessionCookie -> T.Trip -> Handler NoContent
    createTrip cookie trip@T.Trip{..} =
      adminsAnd cookie (\T.Account{..} -> accountUsername == tripUsername) $ do
        liftIO $ Trips.create dbFile trip
        pure NoContent

    updateTrip :: T.SessionCookie -> T.UpdateTripRequest -> Handler NoContent
    updateTrip cookie updates@T.UpdateTripRequest{..} =
      adminsAnd cookie (\T.Account{..} -> accountUsername == T.tripPKUsername updateTripRequestTripPK) $ do
        mTrip <- liftIO $ Trips.get dbFile updateTripRequestTripPK
        case mTrip of
          Nothing -> throwError err400 { errBody = "tripKey is invalid" }
          Just trip@T.Trip{..} -> do
            -- TODO(wpcarro): Prefer function in Trips module that does this in a
            -- DB transaction.
            liftIO $ Trips.delete dbFile updateTripRequestTripPK
            liftIO $ Trips.create dbFile (T.updateTrip updates trip)
            pure NoContent

    deleteTrip :: T.SessionCookie -> T.TripPK -> Handler NoContent
    deleteTrip cookie tripPK@T.TripPK{..} =
      adminsAnd cookie (\T.Account{..} -> accountUsername == tripPKUsername) $ do
      liftIO $ Trips.delete dbFile tripPK
      pure NoContent

    listTrips :: T.SessionCookie -> Handler [T.Trip]
    listTrips cookie = do
      mAccount <- liftIO $ Auth.accountFromCookie dbFile cookie
      case mAccount of
        Nothing -> throwError err401 { errBody = "Your session cookie is invalid. Try logging out and logging back in." }
        Just T.Account{..} ->
          case accountRole of
            T.Admin -> liftIO $ Trips.listAll dbFile
            _ -> liftIO $ Trips.list dbFile accountUsername

    login :: T.AccountCredentials
          -> Handler (Headers '[Header "Set-Cookie" SetCookie] T.Session)
    login (T.AccountCredentials username password) = do
      mAccount <- liftIO $ Accounts.lookup dbFile username
      case mAccount of
        Just account@T.Account{..} -> do
          mAttempts <- liftIO $ LoginAttempts.forUsername dbFile accountUsername
          case mAttempts of
            Nothing ->
              if T.passwordsMatch password accountPassword then do
                uuid <- liftIO $ Sessions.findOrCreate dbFile account
                pure $ addHeader (Auth.mkCookie uuid)
                  T.Session{ sessionUsername = accountUsername
                           , sessionRole = accountRole
                           }
              else do
                liftIO $ LoginAttempts.increment dbFile username
                throwError err401 { errBody = "Your credentials are invalid" }
            Just attempts ->
              if attempts >= 3 then
                throwError err429
              else if T.passwordsMatch password accountPassword then do
                uuid <- liftIO $ Sessions.findOrCreate dbFile account
                pure $ addHeader (Auth.mkCookie uuid)
                  T.Session{ sessionUsername = accountUsername
                           , sessionRole = accountRole
                           }
              else do
                liftIO $ LoginAttempts.increment dbFile username
                throwError err401 { errBody = "Your credentials are invalid" }

        -- In this branch, the user didn't supply a known username.
        Nothing -> throwError err401 { errBody = "Your credentials are invalid" }

    logout :: T.SessionCookie
           -> Handler (Headers '[Header "Set-Cookie" SetCookie] NoContent)
    logout cookie = do
      case Auth.uuidFromCookie cookie of
        Nothing ->
          pure $ addHeader Auth.emptyCookie NoContent
        Just uuid -> do
          liftIO $ Sessions.delete dbFile uuid
          pure $ addHeader Auth.emptyCookie NoContent

    unfreezeAccount :: T.SessionCookie
                    -> T.UnfreezeAccountRequest
                    -> Handler NoContent
    unfreezeAccount cookie T.UnfreezeAccountRequest{..} =
      adminsAnd cookie (\T.Account{..} -> accountRole == T.Manager) $ do
        liftIO $ LoginAttempts.reset dbFile unfreezeAccountRequestUsername
        pure NoContent

    inviteUser :: T.SessionCookie
               -> T.InviteUserRequest
               -> Handler NoContent
    inviteUser cookie T.InviteUserRequest{..} = adminsOnly cookie $ do
      secretUUID <- liftIO $ T.InvitationSecret <$> Random.randomIO
      liftIO $ Invitations.create dbFile
        secretUUID
        inviteUserRequestEmail
        inviteUserRequestRole
      res <- liftIO $ sendInviteEmail config inviteUserRequestEmail secretUUID
      case res of
        Left _ -> undefined
        Right _ -> pure NoContent

    acceptInvitation :: T.AcceptInvitationRequest -> Handler NoContent
    acceptInvitation T.AcceptInvitationRequest{..} = do
      mInvitation <- liftIO $ Invitations.get dbFile acceptInvitationRequestEmail
      case mInvitation of
        Nothing -> throwError err404 { errBody = "No invitation for email" }
        Just T.Invitation{..} ->
          if invitationSecret == acceptInvitationRequestSecret then do
            liftIO $ Accounts.create dbFile
              acceptInvitationRequestUsername
              acceptInvitationRequestPassword
              invitationEmail
              invitationRole
            pure NoContent
          else
            throwError err401 { errBody = "You are not providing a valid secret" }

run :: T.Config -> IO ()
run config@T.Config{..} =
  Warp.run 3000 (enforceCors $ serve (Proxy @ API) $ server config)
  where
    enforceCors = Cors.cors (const $ Just corsPolicy)
    corsPolicy :: Cors.CorsResourcePolicy
    corsPolicy =
      Cors.simpleCorsResourcePolicy
        { Cors.corsOrigins = Just ([cs configClient], True)
        , Cors.corsMethods = Cors.simpleMethods ++ ["PUT", "PATCH", "DELETE", "OPTIONS"]
        , Cors.corsRequestHeaders = Cors.simpleHeaders ++ ["Content-Type", "Authorization"]
        }
