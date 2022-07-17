{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Control.Applicative
import qualified Crypto.Hash as Crypto
import qualified Data.ByteArray as ByteArray
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.ByteString.Lazy as Lazy
import Data.Functor.Compose
import Data.Int (Int64)
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Database.SQLite.Simple (NamedParam ((:=)))
import qualified Database.SQLite.Simple as Sqlite
import qualified Database.SQLite.Simple.FromField as Sqlite
import qualified Database.SQLite.Simple.QQ as Sqlite
import GHC.TypeLits (Symbol)
import MyPrelude
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified SuperRecord as Rec
import System.IO (stderr)
import Control.Monad.Reader

main :: IO ()
main = do
  withEnv $ \env ->
    Warp.runSettings
      (Warp.defaultSettings & Warp.setPort 7070)
      (api env)

withEnv :: (Env -> IO a) -> IO a
withEnv inner = do
  withSqlite "./data.sqlite" $ \envData -> do
    withSqlite "./wordlist.sqlite" $ \envWordlist -> inner Env {..}

withSqlite :: String -> (Sqlite.Connection -> IO a) -> IO a
withSqlite fileName inner = Sqlite.withConnection fileName $ \conn -> do
  Sqlite.setTrace conn (Just (\msg -> Text.hPutStrLn stderr [fmt|{fileName}: {msg}|]))
  Sqlite.execute conn [Sqlite.sql|PRAGMA foreign_keys = ON|] ()
  inner conn

api :: Env -> Wai.Application
api env req respond = do
  case runHandler (getById <|> insertById) req env of
    Nothing -> respond $ Wai.responseLBS Http.status404 [] "endpoint does not exist."
    Just handler' -> do
      handler' >>= \case
        Left (status, err) -> respond $ Wai.responseLBS status [] (err & toLazyBytes)
        Right (headers, body) ->
          respond $
            Wai.responseLBS
              Http.status200
              headers
              ( body & toLazyBytes
              )

data Env = Env
  { envWordlist :: Sqlite.Connection,
    envData :: Sqlite.Connection
  }

-- | I don’t need any fancy routing in this, so a handler is just something that returns a @Just (IO a)@ if it wants to handle the request.
newtype Handler a
  = Handler ( ReaderT (Wai.Request, Env) (Compose Maybe IO) a )
  deriving newtype (Functor, Applicative, Alternative)

handler :: ((Wai.Request, Env) -> Maybe (IO a)) -> Handler a
handler f = Handler (ReaderT (Compose . f))

runHandler :: Handler a -> Wai.Request -> Env -> Maybe (IO a)
runHandler (Handler handler') req env = getCompose $ handler' & (\readerT -> runReaderT readerT (req, env))

getById ::
  Handler
    ( Either
        (Http.Status, ByteString)
        ([(Http.HeaderName, ByteString)], ByteString)
    )
getById = handler $ \(req, env) -> do
  guard ((req & Wai.requestMethod) == Http.methodGet)
  case req & Wai.pathInfo of
    ["v0", "by-id", filename] -> Just $ do
      Sqlite.queryNamed
        @( Rec.Rec
             [ "mimetype" Rec.:= Text,
               "content" Rec.:= ByteString,
               "size" Rec.:= Int
             ]
         )
        (env & envData)
        [Sqlite.sql|
        SELECT
          mimetype,
          cast (content AS blob) as content,
          size
        FROM file_content
        JOIN file_references
          ON file_references.file_content = file_content.hash_sha256
        WHERE
          file_references.reference_type = 'by-id'
          AND (file_references.name || file_references.extension) = :filename
       |]
        [":filename" Sqlite.:= filename]
        <&> \case
          [] -> Left (Http.status404, "File not found.")
          [res] ->
            Right
              ( [ ("Content-Type", res & Rec.get #mimetype & textToBytesUtf8),
                  ("Content-Length", res & Rec.get #size & showToText & textToBytesUtf8)
                ],
                -- TODO: should this be lazy/streamed?
                res & Rec.get #content
              )
          _more -> Left "file_references must be unique (in type and name)" & unwrapError
    _ -> Nothing

insertById :: Handler (Either a ([(Http.HeaderName, ByteString)], ByteString))
insertById = handler $ \(req, env) -> do
  guard ((req & Wai.requestMethod) == Http.methodPost)
  case req & Wai.pathInfo of
    ["v0", "by-id"] -> Just $ do
      let maybeText bytes = case bytesToTextUtf8 bytes of
            Left _err -> Nothing
            Right t -> Just t
      let mimeType =
            ( (req & Wai.requestHeaders & List.lookup "X-Cas-Serve-Mimetype" >>= maybeText)
                <|> (req & Wai.requestHeaders & List.lookup "Content-Type" >>= maybeText)
            )
              & fromMaybe "application/octet-stream"

      let magicFileEnding mimeType' = case Text.split (== '/') mimeType' of
            [_, ""] -> Nothing
            ["", _] -> Nothing
            [_, "any"] -> Nothing
            ["image", ty] -> Just (Text.cons '.' ty)
            ["video", ty] -> Just (Text.cons '.' ty)
            ["text", "plain"] -> Just ".txt"
            ["text", "html"] -> Just ".html"
            ["application", "pdf"] -> Just ".pdf"
            ["application", "json"] -> Just ".json"
            _ -> Nothing

      let extension =
            ( (req & Wai.requestHeaders & List.lookup "X-Cas-Serve-FileExtension" >>= maybeText)
                <|> ( (req & Wai.requestHeaders & List.lookup "Content-Type")
                        >>= maybeText
                        >>= magicFileEnding
                    )
            )
              -- Just the empty extension if we can’t figure it out.
              & fromMaybe ""

      body <- Wai.consumeRequestBodyStrict req
      let hash :: Crypto.Digest Crypto.SHA256 = Crypto.hashlazy body
      let hashBytes = hash & ByteArray.convert @(Crypto.Digest Crypto.SHA256) @ByteString
      let len = ByteString.Lazy.length body
      name <- getNameFromWordlist env
      let fullname = name <> extension

      let conn = env & envData
      Sqlite.withTransaction conn $ do
        Sqlite.executeNamed
          conn
          [Sqlite.sql|
            INSERT INTO file_content
              (content, hash_sha256, size)
              VALUES
              (:content, :hash_sha256, :size)
              ON CONFLICT (hash_sha256) DO NOTHING
          |]
          [ ":content" := (body :: Lazy.ByteString),
            ":hash_sha256" := (hashBytes :: ByteString),
            ":size" := (len :: Int64)
          ]

        -- TODO: we are not checking if the name already exists,
        -- we just assume that 1633^3 is enough to not get any collisions for now.
        -- If the name exists, the user gets a 500.
        Sqlite.executeNamed
          conn
          [Sqlite.sql|
            INSERT INTO file_references
              (file_content, reference_type, name, extension, mimetype)
            VALUES
              (:file_content, :reference_type, :name, :extension, :mimetype)
          |]
          [ ":file_content" := (hashBytes :: ByteString),
            ":reference_type" := ("by-id" :: Text),
            ":name" := name,
            ":extension" := (extension :: Text),
            ":mimetype" := (mimeType :: Text)
          ]
      pure $
        Right
          ( [("Content-Type", "text/plain")],
            [fmt|/v0/by-id/{fullname}|]
          )
    _ -> Nothing

-- Get a random name from a wordlist, that is three words connected by @-@.
getNameFromWordlist :: Env -> IO Text
getNameFromWordlist env =
  do
    let numberOfWords = 3 :: Int
    Sqlite.queryNamed @(Sqlite.Only Text)
      (env & envWordlist)
      [Sqlite.sql|SELECT word FROM wordlist ORDER BY RANDOM() LIMIT :words|]
      [":words" Sqlite.:= numberOfWords]
    <&> map Sqlite.fromOnly
    <&> Text.intercalate "-"

-- | We can use a Rec with a named list of types to parse a returning row of sqlite!!
instance
  ( Rec.UnsafeRecBuild rec rec FromFieldC
  ) =>
  Sqlite.FromRow (Rec.Rec rec)
  where
  fromRow = do
    Rec.unsafeRecBuild @rec @rec @FromFieldC (\_lbl _proxy -> Sqlite.field)

class (Sqlite.FromField a) => FromFieldC (lbl :: Symbol) a

instance (Sqlite.FromField a) => FromFieldC lbl a
