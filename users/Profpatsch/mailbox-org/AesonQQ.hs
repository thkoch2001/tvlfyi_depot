{-# LANGUAGE TemplateHaskellQuotes #-}

module AesonQQ where

import Data.Aeson qualified as Json
import Data.Either qualified as Either
import PossehlAnalyticsPrelude
import PyF qualified
import PyF.Internal.QQ qualified as PyFConf

aesonQQ =
  PyF.mkFormatter
    "aesonQQ"
    PyF.defaultConfig
      { PyFConf.delimiters = Just ('|', '|'),
        PyFConf.postProcess = \exp -> do
          -- TODO: this does not throw an error at compilation time if the json does not parse
          [|
            case Json.eitherDecodeStrict' @Json.Value $ textToBytesUtf8 $ stringToText $(exp) of
              Left err -> error err
              Right a -> a
            |]
      }
