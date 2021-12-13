{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
module Email where
--------------------------------------------------------------------------------
import Data.Text
import Data.String.Conversions (cs)
import Utils

import qualified Mail.Hailgun as MG
import qualified Types as T
--------------------------------------------------------------------------------

newtype SendSuccess = SendSuccess MG.HailgunSendResponse

data SendError
  = MessageError MG.HailgunErrorMessage
  | ResponseError MG.HailgunErrorResponse

-- | Attempt to send an email with `subject` and with message, `body`.
send :: Text
     -> Text
     -> Text
     -> T.Email
     -> IO (Either SendError SendSuccess)
send apiKey subject body (T.Email to) = do
  case mkMsg of
    Left e -> pure $ Left (MessageError e)
    Right x -> do
      res <- MG.sendEmail ctx x
      case res of
        Left e -> pure $ Left (ResponseError e)
        Right y -> pure $ Right (SendSuccess y)
  where
    ctx = MG.HailgunContext { MG.hailgunDomain = "sandboxda5038873f924b50af2f82a0f05cffdf.mailgun.org"
                            , MG.hailgunApiKey = cs apiKey
                            , MG.hailgunProxy = Nothing
                            }
    mkMsg = MG.hailgunMessage
            subject
            (body |> cs |> MG.TextOnly)
            "mailgun@sandboxda5038873f924b50af2f82a0f05cffdf.mailgun.org"
            (MG.MessageRecipients { MG.recipientsTo = [cs to]
                                  , MG.recipientsCC = []
                                  , MG.recipientsBCC = []
                                  })
            []
