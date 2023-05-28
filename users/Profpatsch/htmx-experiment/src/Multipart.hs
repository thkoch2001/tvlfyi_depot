{-# LANGUAGE QuasiQuotes #-}

module Multipart where

import Conduit (ConduitT, MonadResource)
import Conduit qualified as Cond
import Control.Monad.Logger (MonadLogger)
import Control.Selective (Selective)
import Data.ByteString qualified as ByteString
import Data.DList (DList)
import Data.DList qualified as DList
import Data.Functor.Compose
import Data.List qualified as List
import FieldParser
import Label
import PossehlAnalyticsPrelude
-- TODO: Use the multipart module from wai-extra
import Servant.Multipart
import Servant.Multipart.API
import ValidationParseT

-- | A parser for a HTTP multipart form (a form sent by the browser)
newtype MultipartParseT backend m a = MultipartParseT
  { unMultipartParseT ::
      MultipartData backend ->
      m (Validation (NonEmpty Error) a)
  }
  deriving
    (Functor, Applicative, Selective)
    via (ValidationParseT (MultipartData backend) m)

-- | After parsing a form, either we get the result or a list of form fields that failed
newtype FormValidation a
  = FormValidation
      (DList FormValidationResult, Maybe a)
  deriving (Functor, Applicative, Selective) via (Compose ((,) (DList FormValidationResult)) Maybe)
  deriving stock (Show)

data FormValidationResult = FormValidationResult
  { hasError :: Maybe Error,
    formFieldName :: Text,
    originalValue :: Text
  }
  deriving stock (Show)

mkFormValidationResult ::
  ( HasField "formFieldName" form Text,
    HasField "originalValue" form Text
  ) =>
  form ->
  Maybe Error ->
  FormValidationResult
mkFormValidationResult form err =
  FormValidationResult
    { hasError = err,
      formFieldName = form.formFieldName,
      originalValue = form.originalValue
    }

eitherToFormValidation ::
  ( HasField "formFieldName" form Text,
    HasField "originalValue" form Text
  ) =>
  form ->
  Either Error a ->
  FormValidation a
eitherToFormValidation form = \case
  Left err ->
    FormValidation $ (DList.singleton $ mkFormValidationResult form (Just err), Nothing)
  Right a ->
    FormValidation $ ((DList.singleton $ mkFormValidationResult form Nothing), Just a)

failFormValidation ::
  ( HasField "formFieldName" form Text,
    HasField "originalValue" form Text
  ) =>
  form ->
  Error ->
  FormValidation a
failFormValidation form err =
  FormValidation (DList.singleton $ mkFormValidationResult form (Just err), Nothing)

-- | Parse the multipart form or throw a user error with a descriptive error message.
parseMultipart ::
  (MonadLogger m, MonadThrow m) =>
  MultipartParseT backend m a ->
  MultipartData backend ->
  m a
parseMultipart parser multipartData =
  runValidationParseTOrUserError "Cannot parse the multipart form" parser multipartData

-- | Parse the field out of the multipart message
field :: Applicative m => Text -> FieldParser Text a -> MultipartParseT backend m a
field fieldName fieldParser = MultipartParseT $ \mp ->
  mp.inputs
    & findMaybe (\input -> if input.iName == fieldName then Just input.iValue else Nothing)
    & annotate [fmt|Field "{fieldName}" does not exist in the multipart form|]
    >>= runFieldParser fieldParser
    & eitherToListValidation
    & pure

-- | Parse the field out of the multipart message
field' :: Applicative m => Text -> FieldParser Text a -> MultipartParseT backend m (FormValidation a)
field' fieldName fieldParser = MultipartParseT $ \mp ->
  mp.inputs
    & findMaybe (\input -> if input.iName == fieldName then Just input.iValue else Nothing)
    & annotate [fmt|Field "{fieldName}" does not exist in the multipart form|]
    <&> ( \originalValue ->
            originalValue
              & runFieldParser fieldParser
              & eitherToFormValidation
                ( T2
                    (label @"formFieldName" fieldName)
                    (label @"originalValue" originalValue)
                )
        )
    & eitherToListValidation
    & pure

-- | Parse the field out of the multipart message, and into a 'Label' of the given name.
fieldLabel :: forall lbl backend m a. Applicative m => Text -> FieldParser Text a -> MultipartParseT backend m (Label lbl a)
fieldLabel fieldName fieldParser = label @lbl <$> field fieldName fieldParser

-- | Parse the field out of the multipart message, and into a 'Label' of the given name.
fieldLabel' :: forall lbl backend m a. Applicative m => Text -> FieldParser Text a -> MultipartParseT backend m (FormValidation (Label lbl a))
fieldLabel' fieldName fieldParser = fmap (label @lbl) <$> field' fieldName fieldParser

-- | parse all fields out of the multipart message, with the same parser
allFields :: Applicative m => FieldParser Input b -> MultipartParseT backend m [b]
allFields fieldParser = MultipartParseT $ \mp ->
  mp.inputs
    & traverseValidate (runFieldParser fieldParser)
    & eitherToValidation
    & pure

-- | Parse a file by name out of the multipart message
file ::
  Applicative m =>
  Text ->
  GetFileContent backend m content ->
  MultipartParseT backend m (MultipartFile content)
file fieldName getContent = MultipartParseT $ \mp ->
  mp.files
    & List.find (\input -> input.fdInputName == fieldName)
    & annotate [fmt|File "{fieldName}" does not exist in the multipart form|]
    & \case
      Left err -> pure $ Failure (singleton err)
      Right filePath -> fileDataToMultipartFile getContent filePath <&> eitherToListValidation

-- | Return all files from the multipart message
allFiles ::
  Applicative m =>
  GetFileContent backend m content ->
  MultipartParseT backend m [MultipartFile content]
allFiles getContent = MultipartParseT $ \mp -> do
  traverseValidateM (fileDataToMultipartFile getContent) mp.files
    <&> eitherToValidation

-- | Ensure there is exactly one file and return it (ignoring the field name)
exactlyOneFile ::
  Applicative m =>
  GetFileContent backend m content ->
  MultipartParseT backend m (MultipartFile content)
exactlyOneFile getContent = MultipartParseT $ \mp ->
  mp.files
    & \case
      [] -> pure $ failParse "Expected to receive a file, but the multipart form did not contain any files"
      [file_] ->
        file_
          & fileDataToMultipartFile getContent
          <&> eitherToListValidation
      more -> pure $ failParse [fmt|Expected to receive exactly one file, but the multipart form contained {List.length more} files|]
  where
    -- \| Fail to parse the multipart form with the given error message.
    failParse :: Text -> Validation (NonEmpty Error) a
    failParse = Failure . singleton . newError

newtype GetFileContent backend m content = GetFileContent
  {unGetFileContent :: (MultipartResult backend -> m (Either Error content))}

-- | Get the 'FilePath' of the temporary file on disk.
--
-- __ATTN__: Must be consumed before the handler returns, otherwise the temporary file is deleted!
tmpFilePath :: Applicative m => GetFileContent Tmp m FilePath
tmpFilePath = GetFileContent $ \filePath -> pure $ Right $ filePath

tmpFileContent :: MonadIO m => GetFileContent Tmp m ByteString
tmpFileContent =
  -- \| TODO: potentially catch file reading exceptions :P
  GetFileContent $ \filePath -> liftIO $ Right <$> ByteString.readFile filePath

-- | Streams the contents of the file.
--
-- __ATTN__: Must be consumed before the handler returns, otherwise the temporary file is deleted!
-- (Although I can’t figure out whether the handle stays open so it might not be that bad; just don’t move it to a different thread.)
tmpFileContentStream :: (MonadResource io, Applicative m) => GetFileContent Tmp m (ConduitT () ByteString io ())
tmpFileContentStream =
  -- \| TODO: potentially catch file reading exceptions :P
  GetFileContent $ \filePath -> pure $ Right $ Cond.sourceFile filePath

-- | A file field in a multipart message.
data MultipartFile content = MultipartFile
  { -- | @name@ attribute of the corresponding HTML @\<input\>@
    multipartNameAttribute :: Text,
    -- | name of the file on the client's disk
    fileNameOnDisk :: Text,
    -- | MIME type for the file
    fileMimeType :: Text,
    -- | Content of the file
    content :: content
  }

-- | Convert the multipart library struct of a multipart file to our own.
fileDataToMultipartFile ::
  Functor f =>
  GetFileContent backend f content ->
  FileData backend ->
  f (Either Error (MultipartFile content))
fileDataToMultipartFile getContent file_ = runExceptT $ do
  content <- ExceptT $ getContent.unGetFileContent file_.fdPayload
  pure $
    MultipartFile
      { multipartNameAttribute = file_.fdInputName,
        fileNameOnDisk = file_.fdFileName,
        fileMimeType = file_.fdFileCType,
        ..
      }
