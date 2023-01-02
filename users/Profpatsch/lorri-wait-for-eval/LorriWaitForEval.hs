{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Conduit
import Conduit qualified as Cond
import Control.Concurrent
import Control.Concurrent.Async qualified as Async
import Control.Monad
import Data.Aeson.BetterErrors qualified as Json
import Data.Bifunctor
import Data.ByteString (ByteString)
import Data.Conduit.Binary qualified as Conduit.Binary
import Data.Conduit.Combinators qualified as Cond
import Data.Conduit.Process
import Data.Error
import Data.Function
import Data.Functor
import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified
import Data.Text.Encoding.Error qualified
import Data.Text.IO (hPutStrLn)
import PyF
import System.Directory qualified as Dir
import System.Environment qualified as Env
import System.Exit qualified as Exit
import System.FilePath (takeDirectory)
import System.FilePath.Posix qualified as FilePath
import System.IO (stderr)
import System.Posix qualified as Posix
import Prelude hiding (log)

data LorriEvent = LorriEvent
  { nixFile :: Text,
    eventType :: LorriEventType
  }
  deriving stock (Show)

data LorriEventType
  = Completed
  | Started
  | Failure
  deriving stock (Show)

main :: IO ()
main = do
  argv <- Env.getArgs <&> nonEmpty

  dir <- Dir.getCurrentDirectory
  shellNix <-
    findShellNix dir >>= \case
      Nothing -> Exit.die [fmt|could not find any shell.nix in or above the directory {dir}|]
      Just s -> pure s
  getEventChan :: MVar (Chan LorriEvent) <- newEmptyMVar
  Async.race_
    ( do
        sendEventChan :: Chan LorriEvent <- newChan
        (exitCode, ()) <-
          sourceProcessWithConsumer
            (proc "lorri" ["internal", "stream-events"])
            $
            -- first, we want to send a message over the chan that the process is running (for timeout)
            liftIO (putMVar getEventChan sendEventChan)
              *> Conduit.Binary.lines
              .| Cond.mapC
                ( \jsonBytes ->
                    (jsonBytes :: ByteString)
                      & Json.parseStrict
                        ( Json.key
                            "Completed"
                            ( do
                                nixFile <- Json.key "nix_file" Json.asText
                                pure LorriEvent {nixFile, eventType = Completed}
                            )
                            Json.<|> Json.key
                              "Started"
                              ( do
                                  nixFile <- Json.key "nix_file" Json.asText
                                  pure LorriEvent {nixFile, eventType = Started}
                              )
                            Json.<|> Json.key
                              "Failure"
                              ( do
                                  nixFile <- Json.key "nix_file" Json.asText
                                  pure LorriEvent {nixFile, eventType = Failure}
                              )
                        )
                      & first Json.displayError'
                      & first (map newError)
                      & first (smushErrors [fmt|Cannot parse line returned by lorri: {jsonBytes & bytesToTextUtf8Lenient}|])
                      & unwrapError
                )
              .| (Cond.mapM_ (\ev -> writeChan sendEventChan ev))

        log [fmt|lorri internal stream-events exited {show exitCode}|]
    )
    ( do
        let waitMs ms = threadDelay (ms * 1000)

        -- log [fmt|Waiting for lorri event for {shellNix}|]

        eventChan <- takeMVar getEventChan

        let isOurEvent ev = FilePath.normalise (ev & nixFile & textToString) == FilePath.normalise shellNix

        let handleEvent ev =
              case ev & eventType of
                Started ->
                  log [fmt|waiting for lorri build to finish|]
                Completed -> do
                  log [fmt|build completed|]
                  exec (inDirenvDir (takeDirectory shellNix) <$> argv)
                Failure -> do
                  log [fmt|evaluation failed! for path {ev & nixFile}|]
                  Exit.exitWith (Exit.ExitFailure 111)

        -- wait for 100ms for the first message from lorri,
        -- or else assume lorri is not building the project yet
        Async.race
          (waitMs 100)
          ( do
              -- find the first event that we can use
              let go = do
                    ev <- readChan eventChan
                    if isOurEvent ev then pure ev else go
              go
          )
          >>= \case
            Left () -> do
              log [fmt|No event received from lorri, assuming this is the first evaluation|]
              exec argv
            Right ch -> handleEvent ch

        runConduit $
          repeatMC (readChan eventChan)
            .| filterC isOurEvent
            .| mapM_C handleEvent
    )
  where
    inDirenvDir dir' argv' = ("direnv" :| ["exec", dir']) <> argv'
    exec = \case
      Just (exe :| args') -> Posix.executeFile exe True args' Nothing
      Nothing -> Exit.exitSuccess

log :: Text -> IO ()
log msg = hPutStrLn stderr [fmt|lorri-wait-for-eval: {msg}|]

-- | Searches from the current directory upwards, until it finds the `shell.nix`.
findShellNix :: FilePath -> IO (Maybe FilePath)
findShellNix curDir = do
  let go :: (FilePath -> IO (Maybe FilePath))
      go dir = do
        let file = dir FilePath.</> "shell.nix"
        Dir.doesFileExist file >>= \case
          True -> pure (Just file)
          False -> do
            let parent = FilePath.takeDirectory dir
            if parent == dir
              then pure Nothing
              else go parent
  go (FilePath.normalise curDir)

textToString :: Text -> String
textToString = Text.unpack

smushErrors :: Foldable t => Text -> t Error -> Error
smushErrors msg errs =
  errs
    -- hrm, pretty printing and then creating a new error is kinda shady
    & foldMap (\err -> "\n- " <> prettyError err)
    & newError
    & errorContext msg

-- | decode a Text from a ByteString that is assumed to be UTF-8,
-- replace non-UTF-8 characters with the replacment char U+FFFD.
bytesToTextUtf8Lenient :: Data.ByteString.ByteString -> Data.Text.Text
bytesToTextUtf8Lenient =
  Data.Text.Encoding.decodeUtf8With Data.Text.Encoding.Error.lenientDecode
