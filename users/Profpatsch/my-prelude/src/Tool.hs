{-# LANGUAGE QuasiQuotes #-}

module Tool where

import Data.Error.Tree
import Label
import PossehlAnalyticsPrelude
import System.Environment qualified as Env
import System.Exit qualified as Exit
import System.FilePath ((</>))
import System.Posix qualified as Posix
import ValidationParseT

data Tool = Tool
  { -- | absolute path to the executable
    toolPath :: FilePath
  }
  deriving stock (Show)

-- | Reads all tools from the @toolsEnvVar@ variable or aborts.
readTools ::
  Label "toolsEnvVar" Text ->
  -- | Parser for Tools we bring with us at build time.
  --
  -- These are executables that we need available, and that we have to ship with the distribution of @pa-cli@.
  ToolParserT IO tools ->
  IO tools
readTools env toolParser =
  Env.lookupEnv (env.toolsEnvVar & textToString) >>= \case
    Nothing -> do
      Exit.die [fmt|Please set {env.toolsEnvVar} to a directory with all tools we need (see `Tools` in the code).|]
    Just toolsDir ->
      (Posix.fileExist toolsDir & ifTrueOrErr () [fmt|{env.toolsEnvVar} directory does not exist: {toolsDir}|])
        & thenValidateM
          ( \() ->
              (Posix.getFileStatus toolsDir <&> Posix.isDirectory)
                & ifTrueOrErr () [fmt|{env.toolsEnvVar} does not point to a directory: {toolsDir}|]
          )
        & thenValidateM
          (\() -> toolParser.unToolParser toolsDir)
        <&> first (errorTree [fmt|Could not find all tools in {env.toolsEnvVar}|])
        >>= \case
          Failure err -> Exit.die (err & prettyErrorTree & textToString)
          Success t -> pure t

newtype ToolParserT m a = ToolParserT
  { unToolParser ::
      FilePath ->
      m (Validation (NonEmpty Error) a)
  }
  deriving
    (Functor, Applicative)
    via (ValidationParseT FilePath m)

-- | Given a file path and the name of the tool executable, see whether it is an executable and return its full path.
readTool :: Text -> ToolParserT IO Tool
readTool exeName = ToolParserT $ \toolDir -> do
  let toolPath :: FilePath = toolDir </> (exeName & textToString)
  let read' = True
  let write = False
  let exec = True
  Posix.fileExist toolPath
    & ifTrueOrErr () [fmt|Tool does not exist: {toolPath}|]
    & thenValidateM
      ( \() ->
          Posix.fileAccess toolPath read' write exec
            & ifTrueOrErr (Tool {..}) [fmt|Tool is not readable/executable: {toolPath}|]
      )

-- | helper
ifTrueOrErr :: (Functor f) => a -> Text -> f Bool -> f (Validation (NonEmpty Error) a)
ifTrueOrErr true err io =
  io <&> \case
    True -> Success true
    False -> Failure $ singleton $ newError err
