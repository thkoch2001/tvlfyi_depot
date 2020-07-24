{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
module API where
--------------------------------------------------------------------------------
import qualified Data.Pool as DP
import qualified Database.SQLite.Simple as DB

import Data.Aeson
import GHC.Generics
import GHC.TypeLits
import Network.Wai.Handler.Warp
import Servant
import Control.Monad.IO.Class
--------------------------------------------------------------------------------

handlers :: DP.Pool DB.Connection -> Server API
handlers pool = do
  getHandler pool :<|> pure 0

getHandler :: DP.Pool DB.Connection -> Handler Int
getHandler pool =
  DP.withResource pool $ \conn -> do
     result <- liftIO $ DB.query_ conn "select 2 + 2"
     case result of
       [DB.Only x] -> pure x
       _  -> pure (-1)

type API = "number" :> Get '[JSON] Int
      :<|> "other" :> Post '[JSON] Int

main :: IO ()
main = do
  pool <- DP.createPool (DB.open "data.db") DB.close 1 0.5 1
  run 3000 (serve (Proxy @ API) (handlers pool))
