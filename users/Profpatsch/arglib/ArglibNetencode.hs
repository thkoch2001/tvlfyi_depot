{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module ArglibNetencode where

import qualified Data.Attoparsec.ByteString as Atto
import Data.Maybe (fromMaybe)
import ExecHelpers
import MyPrelude
import Netencode
import qualified System.Environment as Env
import qualified System.Posix.Env.ByteString as ByteEnv

arglibNetencode :: CurrentProgramName -> Maybe Text -> IO T
arglibNetencode progName mEnvvar = do
  let envvar = mEnvvar & fromMaybe "ARGLIB_NETENCODE" & textToBytesUtf8
  ByteEnv.getEnv envvar >>= \case
    Nothing -> dieUserError progName [fmt|could not read args, envvar {envvar} not set|]
    Just bytes ->
      case Atto.parseOnly (Netencode.netencodeParser <* Atto.endOfInput) bytes of
        Left err -> dieEnvironmentProblem progName [fmt|arglib parsing error: {err}|]
        Right t -> do
          ByteEnv.unsetEnv envvar
          pure t
