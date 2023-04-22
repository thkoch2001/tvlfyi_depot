{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module RunCommand where

import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as Bytes.Lazy
import Data.Char qualified as Char
import Data.List qualified as List
import Data.Text qualified as Text
import MyPrelude
import System.Exit qualified as Exit
import System.IO (Handle)
import System.Process.Typed qualified as Process
import Prelude hiding (log)

-- | Given a a command, the executable and arguments,
-- spawn the tool as subprocess and collect its stdout (stderr will go to our stderr).

-- Will strip the stdout of trailing newlines.
--
-- If the executable is not a path, it will be resolved via the @PATH@ environment variable.
runCommand :: MonadIO m => FilePath -> [Text] -> m (Exit.ExitCode, ByteString)
runCommand executable args = do
  let bashArgs = prettyArgsForBash ((executable & stringToText) : args)
  log [fmt|Running: $ {bashArgs}|]
  Process.proc
    executable
    (args <&> textToString)
    & Process.readProcessStdout
    <&> second toStrictBytes
    <&> second stripWhitespaceFromEnd

-- | Given a a command, the executable and arguments,
-- spawn the tool as subprocess and run it to conclusion.
--
-- If the executable is not a path, it will be resolved via the @PATH@ environment variable.
runCommandNoStdout :: MonadIO m => FilePath -> [Text] -> m Exit.ExitCode
runCommandNoStdout executable args = do
  let bashArgs = prettyArgsForBash ((executable & stringToText) : args)
  log [fmt|Running: $ {bashArgs}|]
  Process.proc
    executable
    (args <&> textToString)
    & Process.runProcess

-- TODO: This is reversing the whole string *twice*. Can we strip from end without doing that?
stripWhitespaceFromEnd :: ByteString -> ByteString
stripWhitespaceFromEnd = ByteString.reverse . ByteString.dropWhile (\w -> w == charToWordUnsafe '\n') . ByteString.reverse

-- | Like `runCommand`, but takes a Bytestring that provides the command with streamed input on stdin.
runCommandWithStdin :: MonadIO m => FilePath -> [Text] -> Bytes.Lazy.ByteString -> m (Exit.ExitCode, ByteString)
runCommandWithStdin executable args stdin = do
  let bashArgs = prettyArgsForBash ((executable & stringToText) : args)
  log [fmt|Running: $ {bashArgs}|]
  Process.proc
    executable
    (args <&> textToString)
    & Process.setStdin (Process.byteStringInput stdin)
    & Process.readProcessStdout
    <&> second toStrictBytes
    <&> second stripWhitespaceFromEnd

-- | Like `runCommand`, but takes a Bytestring that provides the command with streamed input on stdin.
runCommandWithStdinNoStdout :: MonadIO m => FilePath -> [Text] -> Bytes.Lazy.ByteString -> m Exit.ExitCode
runCommandWithStdinNoStdout executable args stdin = do
  let bashArgs = prettyArgsForBash ((executable & stringToText) : args)
  log [fmt|Running: $ {bashArgs}|]
  Process.proc
    executable
    (args <&> textToString)
    & Process.setStdin (Process.byteStringInput stdin)
    & Process.runProcess

-- | Like 'runCommandWithStdin' but exit if the command returns a non-0 status.
runCommandWithStdinExpect0 :: MonadIO m => FilePath -> [Text] -> Bytes.Lazy.ByteString -> m ByteString
runCommandWithStdinExpect0 executable args stdin =
  runCommandWithStdin executable args stdin >>= \case
    (ex, stdout) -> do
      checkStatus0 executable ex
      pure stdout

-- | Like 'runCommandWithStdinNoStdout' but exit if the command returns a non-0 status.
runCommandWithStdinNoStdoutExpect0 :: MonadIO m => FilePath -> [Text] -> Bytes.Lazy.ByteString -> m ()
runCommandWithStdinNoStdoutExpect0 executable args stdin =
  runCommandWithStdinNoStdout executable args stdin
    >>= checkStatus0 executable

-- | Like 'runCommandExpect0', but don’t capture stdout,
-- connect stdin and stdout to the command until it returns.
--
-- This is for interactive subcommands.
runCommandInteractiveExpect0 :: MonadIO m => FilePath -> [Text] -> m ()
runCommandInteractiveExpect0 executable args = do
  let bashArgs = prettyArgsForBash ((executable & stringToText) : args)
  log [fmt|Running interactively: $ {bashArgs}|]
  ( liftIO $
      Process.runProcess $
        Process.proc
          executable
          (args <&> textToString)
    )
    >>= checkStatus0 executable

-- | Given a name of a command, the executable and arguments,
-- spawn the tool as subprocess and pipe its stdout to the given 'Handle'.
--
-- If the executable is not a path, it will be resolved via the @PATH@ environment variable.
runCommandPipeToHandle :: MonadIO m => FilePath -> [Text] -> Handle -> m Exit.ExitCode
runCommandPipeToHandle executable args handle = do
  -- TODO log the output file?
  let bashArgs = prettyArgsForBash ((executable & stringToText) : args)
  log [fmt|Running: $ {bashArgs}|]
  liftIO $
    Process.runProcess
      ( Process.proc
          executable
          (args <&> textToString)
          & Process.setStdout (Process.useHandleClose handle)
      )

-- | Check whether a command exited 0 or crash.
checkStatus0 :: MonadIO m => FilePath -> Exit.ExitCode -> m ()
checkStatus0 executable = \case
  Exit.ExitSuccess -> pure ()
  Exit.ExitFailure status -> do
    logCritical [fmt|Command `{executable}` did not exit with status 0 (success), but status {status}|]

log :: MonadIO m => Text -> m ()
log = liftIO . putStderrLn

-- | Log the message on the normal logging level & exit the program
logCritical :: MonadIO m => Text -> m b
logCritical msg = do
  liftIO $ putStderrLn msg
  liftIO $ Exit.exitWith (Exit.ExitFailure 1)

-- | Pretty print a command line in a way that can be copied to bash.
prettyArgsForBash :: [Text] -> Text
prettyArgsForBash = Text.intercalate " " . map simpleBashEscape

-- | Simple escaping for bash words. If they contain anything that’s not ascii chars
-- and a bunch of often-used special characters, put the word in single quotes.
simpleBashEscape :: Text -> Text
simpleBashEscape t = do
  case Text.find (not . isSimple) t of
    Just _ -> escapeSingleQuote t
    Nothing -> t
  where
    -- any word that is just ascii characters is simple (no spaces or control characters)
    -- or contains a few often-used characters like - or .
    isSimple c =
      Char.isAsciiLower c
        || Char.isAsciiUpper c
        || Char.isDigit c
        -- These are benign, bash will not interpret them as special characters.
        || List.elem c ['-', '.', ':', '/']
    -- Put the word in single quotes
    -- If there is a single quote in the word,
    -- close the single quoted word, add a single quote, open the word again
    escapeSingleQuote t' = "'" <> Text.replace "'" "'\\''" t' <> "'"
