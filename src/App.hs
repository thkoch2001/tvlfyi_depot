{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
--------------------------------------------------------------------------------
module App where
--------------------------------------------------------------------------------
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStderrLoggingT)
import Database.Persist.Sqlite ( ConnectionPool, createSqlitePool
                               , runSqlPool, runSqlPersistMPool
                               , runMigration, selectFirst, (==.)
                               , insert, entityVal)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Network.Wai.Handler.Warp as Warp
import Servant

import API
import qualified Types as T
--------------------------------------------------------------------------------

server :: ConnectionPool -> Server API
server pool =
  userAddH :<|> userGetH
  where
    userAddH newUser = liftIO $ userAdd newUser
    userGetH name    = liftIO $ userGet name

    userAdd :: T.Account -> IO (Maybe T.Session)
    userAdd newUser = flip runSqlPersistMPool pool $ do
      exists <- selectFirst [T.AccountUsername ==. (T.accountUsername newUser)] []
      case exists of
        Nothing -> do
          insert newUser
          pure $ Just (T.Session { T.username = T.Username "wpcarro"
                                 , T.password = T.Password "testing"
                                 , T.role = T.RegularUser
                                 })
        Just _ -> pure Nothing

    userGet :: Text -> IO (Maybe T.Account)
    userGet name = flip runSqlPersistMPool pool $ do
      mUser <- selectFirst [T.AccountUsername ==. name] []
      pure $ entityVal <$> mUser

app :: ConnectionPool -> Application
app pool = serve (Proxy @ API) $ server pool

mkApp :: FilePath -> IO Application
mkApp sqliteFile = do
  pool <- runStderrLoggingT $ do
    createSqlitePool (cs sqliteFile) 5

  runSqlPool (runMigration T.migrateAll) pool
  pure $ app pool

run :: FilePath -> IO ()
run sqliteFile =
  Warp.run 3000 =<< mkApp sqliteFile
