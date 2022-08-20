{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module ExecHelpers where

import Data.String (IsString)
import MyPrelude
import qualified System.Exit as Sys

newtype CurrentProgramName = CurrentProgramName { unCurrentProgramName :: Text }
  deriving newtype (Show, Eq, Ord, IsString)

-- | Exit 1 to signify a generic expected error
-- (e.g. something that sometimes just goes wrong, like a nix build).
dieExpectedError :: CurrentProgramName -> Text -> IO a
dieExpectedError = dieWith 1

-- | Exit 100 to signify a user error (“the user is holding it wrong”).
--  This is a permanent error, if the program is executed the same way
-- it should crash with 100 again.
dieUserError :: CurrentProgramName -> Text -> IO a
dieUserError = dieWith 100

-- |  Exit 101 to signify an unexpected crash (failing assertion or panic).
diePanic :: CurrentProgramName -> Text -> IO a
diePanic = dieWith 101

-- | Exit 111 to signify a temporary error (such as resource exhaustion)
dieTemporary :: CurrentProgramName -> Text -> IO a
dieTemporary = dieWith 111

-- |  Exit 126 to signify an environment problem
-- (the user has set up stuff incorrectly so the program cannot work)
dieEnvironmentProblem :: CurrentProgramName -> Text -> IO a
dieEnvironmentProblem = dieWith 126

-- | Exit 127 to signify a missing executable.
dieMissingExecutable :: CurrentProgramName -> Text -> IO a
dieMissingExecutable = dieWith 127

dieWith :: Natural -> CurrentProgramName -> Text -> IO a
dieWith status currentProgramName msg = do
  putStderrLn [fmt|{currentProgramName & unCurrentProgramName}: {msg}|]
  Sys.exitWith
    (Sys.ExitFailure (status & fromIntegral @Natural @Int))
