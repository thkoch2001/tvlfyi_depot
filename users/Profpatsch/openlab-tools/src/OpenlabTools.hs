{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module OpenlabTools where

import Control.DeepSeq (NFData, deepseq)
import Control.Monad.Logger qualified as Logger
import Control.Monad.Logger.CallStack
import Control.Monad.Reader
import Data.Aeson.BetterErrors qualified as Json
import Data.Error.Tree
import Data.HashMap.Strict qualified as HashMap
import Data.List qualified as List
import Data.Maybe (listToMaybe)
import Data.Text qualified as Text
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Clock (addUTCTime)
import Debug.Trace
import GHC.Records (HasField (..))
import GHC.Stack qualified
import IHP.HSX.QQ (hsx)
import Json qualified
import Label
import Network.HTTP.Client.Conduit qualified as Http
import Network.HTTP.Simple qualified as Http
import Network.HTTP.Types
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Parse qualified as Wai
import OpenTelemetry.Trace qualified as Otel hiding (getTracer, inSpan, inSpan')
import OpenTelemetry.Trace.Core qualified as Otel hiding (inSpan, inSpan')
import OpenTelemetry.Trace.Monad qualified as Otel
import PossehlAnalyticsPrelude
import Pretty
import System.Environment qualified as Env
import System.IO qualified as IO
import Text.Blaze.Html.Renderer.Pretty qualified as Html.Pretty
import Text.Blaze.Html.Renderer.Utf8 qualified as Html
import Text.Blaze.Html5 qualified as Html
import Text.HTML.TagSoup qualified as Soup
import UnliftIO
import Prelude hiding (span, until)

mainPage :: Html.Html
mainPage =
  Html.docTypeHtml
    [hsx|
          <head>
            <title>Openlab Augsburg Tools</title>
            <meta charset="utf-8">
            <meta name="viewport" content="width=device-width, initial-scale=1">
          </head>

          <body>
            <p>Welcome to the OpenLab Augsburg tools thingy. The idea is to provide some services that can be embedded into our other pages.</p>

            <h2>What’s there</h2>
            <ul>
              <li>
                A <a href="snips/table-opening-hours-last-week">table displaying the opening hours last week</a>, courtesy of <a href="https://mapall.space/heatmap/show.php?id=OpenLab+Augsburg">mapall.space</a>.
              </li>
            </ul>


            <h2>Show me the code/how to contribute</h2>

            <p>The source code can be found <a href="https://code.tvl.fyi/tree/users/Profpatsch/openlab-tools">in my user dir in the tvl repo</a>.</p>

            <p>To build the server, clone the repository from <a href="https://code.tvl.fyi/depot.git">https://code.tvl.fyi/depot.git</a>.
            Then <code>cd</code> into <code>users/Profpatsch</code>, run <code>nix-shell</code>.
            </p>

            <p>You can now run the server with <code>cabal repl openlab-tools/`</code> by executing the <code>main</code> function inside the GHC repl. It starts on port <code>9099</code>.
            <br>
            To try out changes to the code, stop the server with <kbd><kbd>Ctrl</kbd>+<kbd>z</kbd></kbd> and type <code>:reload</code>, then <code>main</code> again.
            <br>
            Finally, from within <code>users/Profpatsch</code> you can start a working development environment by installing <var>vscode</var> or <var>vscodium</var> and the <var>Haskell</var> extension. Then run <code>code .</code> from within the directory.
            </p>

            <p>Once you have a patch, <a href="https://matrix.to/#/@profpatsch:augsburg.one">contact me on Matrix</a> or DM me at <code>irc/libera</code>, nick <code>Profpatsch</code>.
            </p>
          </body>
        |]

debug :: Bool
debug = False

runApp :: IO ()
runApp = withTracer $ \tracer -> do
  cache <- newCache ""
  let renderHtml =
        if debug
          then Html.Pretty.renderHtml >>> stringToText >>> textToBytesUtf8 >>> toLazyBytes
          else Html.renderHtml

  let appT = withRunInIO $ \runInIO -> Warp.run 9099 $ \req respond -> do
        let catchAppException act =
              try act >>= \case
                Right a -> pure a
                Left (AppException err) -> do
                  runInIO (logError err)
                  respond (Wai.responseLBS Http.status500 [] "")

        catchAppException $ do
          case req & Wai.pathInfo & Text.intercalate "/" of
            "" -> do
              respond $ Wai.responseLBS Http.status200 [] (renderHtml mainPage)
            "snips/table-opening-hours-last-week" -> do
              new <- runInIO $ updateCacheIfNewer cache heatmap

              respond $ Wai.responseLBS Http.status200 [] (new & toLazyBytes)
            _ -> do respond $ Wai.responseLBS Http.status404 [] "nothing here (yet)"

  runReaderT appT.unAppT Context {..}

heatmap :: AppT IO ByteString
heatmap = do
  Http.httpBS [fmt|GET https://mapall.space/heatmap/show.php?id=OpenLab+Augsburg|]
    <&> (.responseBody)
    <&> Soup.parseTags
    <&> Soup.canonicalizeTags
    <&> findHeatmap
    <&> fromMaybe ""
  where
    firstSection f t = t & Soup.sections f & listToMaybe
    match :: Soup.Tag ByteString -> Soup.Tag ByteString -> Bool
    match x (t :: Soup.Tag ByteString) = (Soup.~==) @ByteString t x
    findHeatmap t =
      t
        & firstSection (match (Soup.TagOpen ("") [("class", "heatmap")]))
        >>= firstSection (match (Soup.TagOpen "table" []))
        <&> getTable
        <&> Soup.renderTags

    -- get the table from opening tag to closing tag (allowing nested tables)
    getTable = go 0
      where
        go _ [] = []
        go d (el : els)
          | match (Soup.TagOpen "table" []) el = el : go (traceShowId $ d + 1) els
          | match (Soup.TagClose "table") el = if d <= 1 then [el] else el : go (traceShowId $ d - 1) els
          | otherwise = el : go d els

main :: IO ()
main =
  runApp

-- ( do
--     -- todo: trace that to the init functions as well
--     Otel.inSpan "whatcd-resolver main function" Otel.defaultSpanArguments $ do
--       _ <- runTransaction migrate
--       htmlUi
-- )

inSpan :: (MonadUnliftIO m, Otel.MonadTracer m) => Text -> m a -> m a
inSpan name = Otel.inSpan name Otel.defaultSpanArguments

inSpan' :: (MonadUnliftIO m, Otel.MonadTracer m) => Text -> (Otel.Span -> m a) -> m a
inSpan' name = Otel.inSpan' name Otel.defaultSpanArguments

zipT2 ::
  forall l1 l2 t1 t2.
  ( HasField l1 (T2 l1 [t1] l2 [t2]) [t1],
    HasField l2 (T2 l1 [t1] l2 [t2]) [t2]
  ) =>
  T2 l1 [t1] l2 [t2] ->
  [T2 l1 t1 l2 t2]
zipT2 xs =
  zipWith
    (\t1 t2 -> T2 (label @l1 t1) (label @l2 t2))
    (getField @l1 xs)
    (getField @l2 xs)

unzipT2 :: forall l1 t1 l2 t2. [T2 l1 t1 l2 t2] -> T2 l1 [t1] l2 [t2]
unzipT2 xs = xs <&> toTup & unzip & fromTup
  where
    toTup :: forall a b. T2 a t1 b t2 -> (t1, t2)
    toTup (T2 a b) = (getField @a a, getField @b b)
    fromTup :: (a, b) -> T2 l1 a l2 b
    fromTup (t1, t2) = T2 (label @l1 t1) (label @l2 t2)

unzipT3 :: forall l1 t1 l2 t2 l3 t3. [T3 l1 t1 l2 t2 l3 t3] -> T3 l1 [t1] l2 [t2] l3 [t3]
unzipT3 xs = xs <&> toTup & unzip3 & fromTup
  where
    toTup :: forall a b c. T3 a t1 b t2 c t3 -> (t1, t2, t3)
    toTup (T3 a b c) = (getField @a a, getField @b b, getField @c c)
    fromTup :: (a, b, c) -> T3 l1 a l2 b l3 c
    fromTup (t1, t2, t3) = T3 (label @l1 t1) (label @l2 t2) (label @l3 t3)

newtype Optional a = OptionalInternal (Maybe a)

mkOptional :: a -> Optional a
mkOptional defaultValue = OptionalInternal $ Just defaultValue

defaults :: Optional a
defaults = OptionalInternal Nothing

instance HasField "withDefault" (Optional a) (a -> a) where
  getField (OptionalInternal m) defaultValue = case m of
    Nothing -> defaultValue
    Just a -> a

httpJson ::
  ( MonadIO m,
    MonadThrow m
  ) =>
  (Optional (Label "contentType" ByteString)) ->
  Otel.Span ->
  Json.Parse ErrorTree b ->
  Http.Request ->
  m b
httpJson opts span parser req = do
  let opts' = opts.withDefault (label @"contentType" "application/json")
  Http.httpBS req
    >>= assertM
      span
      ( \resp -> do
          let statusCode = resp & Http.responseStatus & (.statusCode)
              contentType =
                resp
                  & Http.responseHeaders
                  & List.lookup "content-type"
                  <&> Wai.parseContentType
                  <&> (\(ct, _mimeAttributes) -> ct)
          if
              | statusCode == 200,
                Just ct <- contentType,
                ct == opts'.contentType ->
                  Right $ (resp & Http.responseBody)
              | statusCode == 200,
                Just otherType <- contentType ->
                  Left [fmt|Server returned a non-json body, with content-type "{otherType}"|]
              | statusCode == 200,
                Nothing <- contentType ->
                  Left [fmt|Server returned a body with unspecified content type|]
              | code <- statusCode -> Left [fmt|Server returned an non-200 error code, code {code}: {resp & showPretty}|]
      )
    >>= assertM
      span
      ( \body ->
          Json.parseStrict parser body
            & first (Json.parseErrorTree "could not parse redacted response")
      )

assertM :: (MonadThrow f, MonadIO f) => Otel.Span -> (t -> Either ErrorTree a) -> t -> f a
assertM span f v = case f v of
  Right a -> pure a
  Left err -> appThrowTree span err

data Cache a = Cache
  { until :: !UTCTime,
    result :: !a
  }

newCache :: a -> IO (IORef (Cache a))
newCache result = do
  until <- getCurrentTime
  newIORef Cache {..}

updateCache :: (NFData a) => IORef (Cache a) -> a -> IO ()
updateCache cache result' = do
  -- make sure we don’t hold onto the world by deepseq-ing and evaluating to WHNF
  let !result = deepseq result' result'
  until <- getCurrentTime <&> ((5 * 60) `addUTCTime`)
  _ <- writeIORef cache $! Cache {..}
  pure ()

updateCacheIfNewer :: (MonadUnliftIO m, NFData b) => IORef (Cache b) -> m b -> m b
updateCacheIfNewer cache act = withRunInIO $ \runInIO -> do
  old <- readIORef cache
  now <- getCurrentTime
  if old.until < now
    then do
      res <- runInIO act
      updateCache cache res

      pure res
    else pure old.result

-- pgFormat <- readTools (label @"toolsEnvVar" "OPENLAB_TOOLS_TOOLS") (readTool "pg_format")
-- let config = label @"logDatabaseQueries" LogDatabaseQueries
-- pgConnPool <-
--   Pool.newPool $
--     Pool.defaultPoolConfig
--       {- resource init action -} (Postgres.connectPostgreSQL (db & TmpPg.toConnectionString))
--       {- resource destruction -} Postgres.close
--       {- unusedResourceOpenTime -} 10
--       {- max resources across all stripes -} 20
-- transmissionSessionId <- newEmptyMVar
-- let newAppT = do
--       logInfo [fmt|Running with config: {showPretty config}|]
--       logInfo [fmt|Connected to database at {db & TmpPg.toDataDirectory} on socket {db & TmpPg.toConnectionString}|]
--       appT
-- runReaderT newAppT.unAppT Context {..}

withTracer :: (Otel.Tracer -> IO c) -> IO c
withTracer f = do
  setDefaultEnv "OTEL_SERVICE_NAME" "whatcd-resolver"
  bracket
    -- Install the SDK, pulling configuration from the environment
    Otel.initializeGlobalTracerProvider
    -- Ensure that any spans that haven't been exported yet are flushed
    Otel.shutdownTracerProvider
    -- Get a tracer so you can create spans
    (\tracerProvider -> f $ Otel.makeTracer tracerProvider "whatcd-resolver" Otel.tracerOptions)

setDefaultEnv :: String -> String -> IO ()
setDefaultEnv envName defaultValue = do
  Env.lookupEnv envName >>= \case
    Just _env -> pure ()
    Nothing -> Env.setEnv envName defaultValue

data Context = Context
  { tracer :: Otel.Tracer
  }

newtype AppT m a = AppT {unAppT :: ReaderT Context m a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, MonadThrow)

data AppException = AppException Text
  deriving stock (Show)
  deriving anyclass (Exception)

-- | A specialized variant of @addEvent@ that records attributes conforming to
-- the OpenTelemetry specification's
-- <https://github.com/open-telemetry/opentelemetry-specification/blob/49c2f56f3c0468ceb2b69518bcadadd96e0a5a8b/specification/trace/semantic_conventions/exceptions.md semantic conventions>
--
-- @since 0.0.1.0
recordException ::
  ( MonadIO m,
    HasField "message" r Text,
    HasField "type_" r Text
  ) =>
  Otel.Span ->
  r ->
  m ()
recordException span dat = liftIO $ do
  callStack <- GHC.Stack.whoCreated dat.message
  newEventTimestamp <- Just <$> Otel.getTimestamp
  Otel.addEvent span $
    Otel.NewEvent
      { newEventName = "exception",
        newEventAttributes =
          HashMap.fromList
            [ ("exception.type", Otel.toAttribute @Text dat.type_),
              ("exception.message", Otel.toAttribute @Text dat.message),
              ("exception.stacktrace", Otel.toAttribute @Text $ Text.unlines $ map stringToText callStack)
            ],
        ..
      }

appThrowTree :: (MonadThrow m, MonadIO m) => Otel.Span -> ErrorTree -> m a
appThrowTree span exc = do
  let msg = prettyErrorTree exc
  recordException
    span
    ( T2
        (label @"type_" "AppException")
        (label @"message" msg)
    )
  throwM $ AppException msg

orAppThrowTree :: (MonadThrow m, MonadIO m) => Otel.Span -> Either ErrorTree a -> m a
orAppThrowTree span = \case
  Left err -> appThrowTree span err
  Right a -> pure a

instance (MonadIO m) => MonadLogger (AppT m) where
  monadLoggerLog loc src lvl msg = liftIO $ Logger.defaultOutput IO.stderr loc src lvl (Logger.toLogStr msg)

instance (Monad m) => Otel.MonadTracer (AppT m) where
  getTracer = AppT $ asks (.tracer)
