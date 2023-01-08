{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Aeson (parseErrorTree)
import Control.Exception (try)
import Control.Monad (replicateM)
import Data.Aeson qualified as Json
import Data.Aeson.BetterErrors qualified as Json
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as Char8
import Data.Error.Tree
import Data.Functor.Compose
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import ExecHelpers
import GHC.Records (HasField (..))
import Label
import MyPrelude
import Netencode qualified
import Network.HTTP.Conduit qualified as Client
import Network.HTTP.Simple qualified as Client
import Pretty
import System.Directory qualified as File
import System.Environment qualified as Env
import System.Exit qualified as Exit
import System.FilePath ((</>))
import System.Process qualified as Proc
import System.Random qualified as Random
import System.Random.Stateful qualified as Random
import Prelude hiding (log)
import qualified Netencode.Parse as NetParse

secret :: IO (T2 "email" ByteString "password" ByteString)
secret = do
  T2
    (label @"email" "mail@profpatsch.de")
    <$> (label @"password" <$> fromPass "email/mailbox.org")
  where
    fromPass name =
      Proc.readProcess "pass" [name] ""
        <&> stringToText
        <&> textToBytesUtf8
        <&> Char8.strip

progName :: CurrentProgramName
progName = "mailbox-org"

log :: Error -> IO ()
log err = do
  putStderrLn (errorContext progName.unCurrentProgramName err & prettyError)

data Tools = Tools
  { sieveTest :: Tool
  }
  deriving stock (Show)

newtype Tool = Tool FilePath
  deriving stock Show

parseTools :: Applicative m => (Text -> m (Either Error Tool)) -> m (Either ErrorTree Tools)
parseTools getTool = do
  ( do
        sieveTest <- get "sieve-test"
        pure Tools {..}
    ).getCompose <&> first (errorTree "Error reading tools") <&> validationToEither

  where
   get name = name & getTool <&> eitherToListValidation & Compose
-- | Parse the tools from the given arglib input, and check that the executables exist
parseToolsArglib :: Netencode.T -> IO Tools
parseToolsArglib t = do
  let oneTool name =
        NetParse.asText
          <&> textToString
          <&> ( \path ->
                  path
                    & File.getPermissions
                    <&> File.executable
                    <&> ( \case
                            False -> Left $  [fmt|Tool "{name}" is not an executable|]
                            True -> Right (Tool path)
                        )
              )
  let allTools =
        parseTools (\name -> Compose $ NetParse.key name >>> oneTool name)
          & getCompose
  t
    & NetParse.runParse
      "test"
      -- TODO: a proper ParseT for netencode values
      ( NetParse.asRecord
          >>> NetParse.key "BINS"
          >>> NetParse.asRecord
          >>> allTools
      )
    & orDo diePanic'
    & join @IO
    >>= orDo (\errs -> errs  & diePanic')

-- | Just assume the tools exist by name in the environment.
parseToolsToolname :: IO Tools
parseToolsToolname =
  parseTools
    ( \name ->
        checkInPath name <&> \case
          False -> Left [fmt|"Cannot find "{name}" in PATH|]
          True -> Right $ Tool (name & textToString)
    )
    >>= orDo diePanic'

checkInPath :: Text -> IO Bool
checkInPath name = do
  Env.lookupEnv "PATH"
    <&> annotate "No PATH set"
    >>= orDo diePanic'
    <&> stringToText
    <&> Text.split (== ':')
    <&> filter (/= "")
    >>= traverse
      ( \p ->
          File.getPermissions ((textToString p) </> (textToString name))
            <&> File.executable
            & try @IOError
            >>= \case
              Left _ioError -> pure False
              Right isExe -> pure isExe
      )
    <&> or

diePanic' :: ErrorTree -> IO a
diePanic' errs = errs & prettyErrorTree & diePanic progName

orDo :: Applicative f => (t -> f a) -> Either t a -> f a
orDo f = \case
  Left e -> f e
  Right a -> pure a


main :: IO ()
main =
  secret
    >>= run applyFilters

run ::
  ( HasField "email" dat ByteString,
    HasField "password" dat ByteString
  ) =>
  (Session -> IO ()) ->
  dat ->
  IO ()
run act loginData = do
  session <- login loginData
  act session

listFilterConfig :: Session -> IO ()
listFilterConfig session = do
  mailfilter
    session
    "config"
    mempty
    (Json.key "data" Json.asObject)
    ()
    >>= printPretty

applyFilterRule ::
  ( HasField "folderId" dat Text,
    HasField "rulename" dat Text
  ) =>
  dat ->
  Session ->
  IO ()
applyFilterRule dat session = do
  mailfilter
    session
    "apply"
    ( T2
        (label @"extraQueryParams" [("folderId", Just (dat.folderId & textToBytesUtf8))])
        mempty
    )
    (Json.key "data" Json.asArray >> pure ())
    (Json.Object mempty)

data FilterRule = FilterRule
  { actioncmds :: NonEmpty Json.Object,
    test :: NonEmpty Json.Object
  }

data MailfilterList = MailfilterList
  { id_ :: Json.Value,
    rulename :: Text
  }
  deriving stock (Show, Eq)

applyFilters :: Session -> IO ()
applyFilters session = do
  filters <-
    mailfilter
      session
      "list"
      mempty
      ( Json.key "data" $ do
          ( Json.eachInArray $ asDat @"mailfilter" $ do
              id_ <- Json.key "id" Json.asValue
              rulename <- Json.key "rulename" Json.asText
              pure MailfilterList {..}
            )
            <&> mapFromListOn (\dat -> getLabel @"rulename" dat.parsed)
      )
      ([] :: [()])
  let goal = Map.fromList [(label @"rulename" "another", 32), (label @"rulename" "xyz", 23)]
  let actions = declarativeUpdate goal filters
  log [fmt|To * create: {actions.toCreate & Map.keys & show}, * update: {actions.toUpdate & Map.keys & show}, * delete: {actions.toDelete & Map.keys & show}|]
  where
    -- filters
    --   & Map.elems
    --   & traverse_
    --     ( updateIfDifferent
    --         session
    --         ( \el ->
    --             pure $
    --               el.original.mailfilter
    --                 & KeyMap.insert "active" (Json.Bool False)
    --         )
    --         (pure ())
    --     )

    mapFromListOn :: Ord k => (a -> k) -> [a] -> Map k a
    mapFromListOn on xs = xs <&> (\x -> (on x, x)) & Map.fromList
    updateIfDifferent ::
      forall label parsed.
      ( HasField "id_" parsed Json.Value,
        HasField "rulename" parsed Text
      ) =>
      Session ->
      (Dat label Json.Object parsed -> IO Json.Object) ->
      Json.Parse Error () ->
      Dat label Json.Object parsed ->
      IO ()
    updateIfDifferent session switcheroo parser dat = do
      new <- switcheroo dat
      if new /= getField @label dat.original
        then do
          log [fmt|Updating filter "{dat.parsed.rulename}" (id {dat.parsed.id_ & show @Json.Value})|]
          mailfilter
            session
            "update"
            mempty
            parser
            new
        else do
          log [fmt|Skipping updating filter "{dat.parsed.rulename}" (id {dat.parsed.id_ & show @Json.Value}) because nothing changed.|]

-- | https://oxpedia.org/wiki/index.php?title=HTTP_API_MailFilter
mailfilter ::
  ( Json.ToJSON a,
    Show b
  ) =>
  Session ->
  ByteString ->
  T2
    "extraQueryParams"
    Client.Query
    "httpMethod"
    (Maybe ByteString) ->
  Json.Parse Error b ->
  a ->
  IO b
mailfilter session action opts parser body = do
  req <-
    Client.parseRequest "https://office.mailbox.org/appsuite/api/mailfilter/v2"
      <&> Client.setQueryString
        ( [ ("action", Just action),
            ("colums", Just "1")
          ]
            <> opts.extraQueryParams
        )
      <&> Client.setRequestMethod (opts.httpMethod & fromMaybe "PUT")
      <&> Client.setRequestBodyJSON body
      <&> addSession session
  req
    & httpJSON [fmt|Cannot parse result for {req & prettyRequestShort}|] parser
    >>= okOrDie
    -- >>= (\resp -> printPretty resp >> pure resp)
    <&> Client.responseBody
  where
    prettyRequestShort :: Client.Request -> Text
    prettyRequestShort req = [fmt|request {req & Client.method}: {req & Client.host}{req & Client.path}{req & Client.queryString}|]

-- | Given a goal and the actual state, return which elements to delete, update and create.
declarativeUpdate ::
  Ord k =>
  -- | goal map
  Map k a ->
  -- | actual map
  Map k b ->
  T3
    "toCreate"
    (Map k a)
    "toDelete"
    (Map k b)
    "toUpdate"
    (Map k a)
declarativeUpdate goal actual =
  T3
    (label @"toCreate" $ goal `Map.difference` actual)
    (label @"toDelete" $ actual `Map.difference` goal)
    (label @"toUpdate" $ goal `Map.intersection` actual)

newtype Session = Session Client.CookieJar

httpJSON ::
  Error ->
  Json.Parse Error b ->
  Client.Request ->
  IO (Client.Response b)
httpJSON errMsg parser req = do
  req
    & Client.httpJSON @_ @Json.Value
    >>= traverse
      ( \val -> do
          case val of
            Json.Object obj
              | "error" `KeyMap.member` obj
                  && "error_desc" `KeyMap.member` obj -> do
                  printPretty obj
                  diePanic' "Server returned above inline error"
            _ -> pure ()
          val & Json.parseValue parser & \case
            Left errs ->
              errs
                & parseErrorTree errMsg
                & diePanic'
            Right a -> pure a
      )

data Dat label orig parsed = Dat
  { original :: Label label orig,
    parsed :: parsed
  }
  deriving stock (Show, Eq)

asDat ::
  forall label err m a.
  Monad m =>
  Json.ParseT err m a ->
  Json.ParseT err m (Dat label Json.Object a)
asDat parser = do
  original <- label @label <$> Json.asObject
  parsed <- parser
  pure Dat {..}

addSession :: Session -> Client.Request -> Client.Request
addSession (Session jar) req = do
  let sessionId =
        jar
          & Client.destroyCookieJar
          & List.find (\c -> "open-xchange-session-" `ByteString.isPrefixOf` c.cookie_name)
          & annotate "The cookie jar did not contain an open-exchange-session-*"
          & unwrapError
          & (.cookie_value)

  let req' = req & Client.addToRequestQueryString [("session", Just sessionId)]
  req' {Client.cookieJar = Just jar}

-- | Log into the mailbox.org service, and return the session secret cookies.
login :: (HasField "email" dat ByteString, HasField "password" dat ByteString) => dat -> IO Session
login dat = do
  rnd <- randomString
  req <-
    Client.parseRequest "https://office.mailbox.org/ajax/login"
      <&> Client.setQueryString
        [ ("action", Just "formlogin"),
          ("authId", Just $ ("mbo-" <> rnd) & stringToText & textToBytesUtf8)
        ]
      <&> Client.urlEncodedBody
        [ ("version", "Form+Login"),
          ("autologin", "true"),
          ("client", "open-xchange-appsuite"),
          ("uiWebPath", "/appsuite/"),
          ("login", dat.email),
          ("password", dat.password)
        ]
  Client.httpNoBody req
    >>= okOrDie
    <&> Client.responseCookieJar
    <&> Session
  where
    -- For some reason they want the client to pass a random string
    -- which is used for the session?‽!?
    randomString = do
      gen <- Random.newIOGenM =<< Random.newStdGen
      let chars = ['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9']
      let len = 11
      Random.uniformRM (0, List.length chars - 1) gen
        & replicateM len
        <&> map (\index -> chars !! index)

okOrDie :: Show a => Client.Response a -> IO (Client.Response a)
okOrDie resp =
  case resp & Client.getResponseStatusCode of
    200 -> pure resp
    _ -> do
      printPretty resp
      Exit.die "non-200 result"
