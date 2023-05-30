{-# LANGUAGE QuasiQuotes #-}

module ArglibNetencode where

import Data.Attoparsec.ByteString qualified as Atto
import ExecHelpers
import Label
import Netencode qualified
import PossehlAnalyticsPrelude
import System.Posix.Env.ByteString qualified as ByteEnv

arglibNetencode :: CurrentProgramName -> Maybe (Label "arglibEnvvar" Text) -> IO Netencode.T
arglibNetencode progName mEnvvar = do
  let envvar = mEnvvar <&> (.arglibEnvvar) & fromMaybe "ARGLIB_NETENCODE" & textToBytesUtf8
  ByteEnv.getEnv envvar >>= \case
    Nothing -> dieUserError progName [fmt|could not read args, envvar {envvar} not set|]
    Just bytes ->
      case Atto.parseOnly (Netencode.netencodeParser <* Atto.endOfInput) bytes of
        Left err -> dieEnvironmentProblem progName [fmt|arglib parsing error: {err}|]
        Right t -> do
          ByteEnv.unsetEnv envvar
          pure t
