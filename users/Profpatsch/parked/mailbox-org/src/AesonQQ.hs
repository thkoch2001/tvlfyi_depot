{-# LANGUAGE TemplateHaskellQuotes #-}

module AesonQQ where

import Data.Aeson qualified as Json
import Language.Haskell.TH.Quote (QuasiQuoter)
import PossehlAnalyticsPrelude
import PyF qualified
import PyF.Internal.QQ qualified as PyFConf

aesonQQ :: QuasiQuoter
aesonQQ =
  PyF.mkFormatter
    "aesonQQ"
    PyF.defaultConfig
      { PyFConf.delimiters = Just ('|', '|'),
        PyFConf.postProcess = \exp_ -> do
          -- TODO: this does not throw an error at compilation time if the json does not parse
          [|
            case Json.eitherDecodeStrict' @Json.Value $ textToBytesUtf8 $ stringToText $(exp_) of
              Left err -> error err
              Right a -> a
            |]
      }
