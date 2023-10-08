{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Postgres.MonadPostgres where

import AtLeast (AtLeast)
import Control.Exception
import Control.Monad.Except
import Control.Monad.Logger (MonadLogger, logDebug, logWarn)
import Control.Monad.Reader (MonadReader (ask), ReaderT (..))
import Control.Monad.Trans.Resource
import Data.Aeson (FromJSON)
import Data.Error.Tree
import Data.HashMap.Strict qualified as HashMap
import Data.Int (Int64)
import Data.Kind (Type)
import Data.List qualified as List
import Data.Pool (Pool)
import Data.Pool qualified as Pool
import Data.Text qualified as Text
import Data.Typeable (Typeable)
import Database.PostgreSQL.Simple (Connection, FormatError, FromRow, Query, QueryError, ResultError, SqlError, ToRow)
import Database.PostgreSQL.Simple qualified as PG
import Database.PostgreSQL.Simple qualified as Postgres
import Database.PostgreSQL.Simple.FromRow qualified as PG
import Database.PostgreSQL.Simple.ToField (ToField)
import Database.PostgreSQL.Simple.ToRow (ToRow (toRow))
import Database.PostgreSQL.Simple.Types (Query (..))
import GHC.Records (HasField (..))
import Label
import OpenTelemetry.Trace.Core qualified as Otel hiding (inSpan, inSpan')
import OpenTelemetry.Trace.Monad qualified as Otel
import PossehlAnalyticsPrelude
import Postgres.Decoder
import Postgres.Decoder qualified as Dec
import Pretty (showPretty)
import Seconds
import System.Exit (ExitCode (..))
import Tool
import UnliftIO (MonadUnliftIO (withRunInIO))
import UnliftIO.Process qualified as Process
import UnliftIO.Resource qualified as Resource
import Prelude hiding (span)

-- | Postgres queries/commands that can be executed within a running transaction.
--
-- These are implemented with the @postgresql-simple@ primitives of the same name
-- and will behave the same unless othewise documented.
class (Monad m) => MonadPostgres (m :: Type -> Type) where
  -- | Execute an INSERT, UPDATE, or other SQL query that is not expected to return results.

  -- Returns the number of rows affected.
  execute :: (ToRow params, Typeable params) => Query -> params -> Transaction m (Label "numberOfRowsAffected" Natural)

  -- | Execute an INSERT, UPDATE, or other SQL query that is not expected to return results. Does not take parameters.

  -- Returns the number of rows affected.
  execute_ :: Query -> Transaction m (Label "numberOfRowsAffected" Natural)

  -- | Execute a multi-row INSERT, UPDATE, or other SQL query that is not expected to return results.
  --
  -- Returns the number of rows affected. If the list of parameters is empty, this function will simply return 0 without issuing the query to the backend. If this is not desired, consider using the 'PG.Values' constructor instead.
  executeMany :: (ToRow params, Typeable params) => Query -> [params] -> Transaction m (Label "numberOfRowsAffected" Natural)

  -- | Execute INSERT ... RETURNING, UPDATE ... RETURNING, or other SQL query that accepts multi-row input and is expected to return results. Note that it is possible to write query conn "INSERT ... RETURNING ..." ... in cases where you are only inserting a single row, and do not need functionality analogous to 'executeMany'.
  --
  -- If the list of parameters is empty, this function will simply return [] without issuing the query to the backend. If this is not desired, consider using the 'PG.Values' constructor instead.
  executeManyReturningWith :: (ToRow q) => Query -> [q] -> Decoder r -> Transaction m [r]

  -- | Run a query, passing parameters and result row parser.
  queryWith :: (PG.ToRow params, Typeable params, Typeable r) => PG.Query -> params -> Decoder r -> Transaction m [r]

  -- | Run a query without any parameters and result row parser.
  queryWith_ :: (Typeable r) => PG.Query -> Decoder r -> Transaction m [r]

  -- | Run a query, passing parameters, and fold over the resulting rows.
  --
  -- This doesn’t have to realize the full list of results in memory,
  -- rather results are streamed incrementally from the database.
  --
  -- When dealing with small results, it may be simpler (and perhaps faster) to use query instead.
  --
  -- This fold is _not_ strict. The stream consumer is responsible for forcing the evaluation of its result to avoid space leaks.
  --
  -- If you can, prefer aggregating in the database itself.
  foldRows ::
    (FromRow row, ToRow params, Typeable row, Typeable params) =>
    Query ->
    params ->
    a ->
    (a -> row -> Transaction m a) ->
    Transaction m a

  -- | Run a given transaction in a transaction block, rolling back the transaction
  -- if any exception (postgres or Haskell Exception) is thrown during execution.
  --
  -- Re-throws the exception.
  --
  -- Don’t do any long-running things on the Haskell side during a transaction,
  -- because it will block a database connection and potentially also lock
  -- database tables from being written or read by other clients.
  --
  -- Nonetheless, try to push transactions as far out to the handlers as possible,
  -- don’t do something like @runTransaction $ query …@, because it will lead people
  -- to accidentally start nested transactions (the inner transaction is run on a new connections,
  -- thus can’t see any changes done by the outer transaction).
  -- Only handlers should run transactions.
  runTransaction :: Transaction m a -> m a

-- | Run a query, passing parameters.
query :: forall m params r. (PG.ToRow params, PG.FromRow r, Typeable params, Typeable r, MonadPostgres m) => PG.Query -> params -> Transaction m [r]
query qry params = queryWith qry params (Decoder PG.fromRow)

-- | Run a query without any parameters.
query_ :: forall m r. (Typeable r, PG.FromRow r, MonadPostgres m) => PG.Query -> Transaction m [r]
query_ qry = queryWith_ qry (Decoder PG.fromRow)

-- TODO: implement via fold, so that the result doesn’t have to be realized in memory
querySingleRow ::
  ( MonadPostgres m,
    ToRow qParams,
    Typeable qParams,
    FromRow a,
    Typeable a,
    MonadThrow m
  ) =>
  Query ->
  qParams ->
  Transaction m a
querySingleRow qry params = do
  query qry params >>= ensureSingleRow

-- TODO: implement via fold, so that the result doesn’t have to be realized in memory
querySingleRowMaybe ::
  ( MonadPostgres m,
    ToRow qParams,
    Typeable qParams,
    FromRow a,
    Typeable a,
    MonadThrow m
  ) =>
  Query ->
  qParams ->
  Transaction m (Maybe a)
querySingleRowMaybe qry params = do
  rows <- query qry params
  case rows of
    [] -> pure Nothing
    [one] -> pure (Just one)
    -- TODO: Should we MonadThrow this here? It’s really an implementation detail of MonadPostgres
    -- that a database function can error out, should probably handled by the instances.
    more -> throwM $ SingleRowError {numberOfRowsReturned = (List.length more)}

ensureSingleRow :: (MonadThrow m) => [a] -> m a
ensureSingleRow = \case
  -- TODO: Should we MonadThrow this here? It’s really an implementation detail of MonadPostgres
  -- that a database function can error out, should probably handled by the instances.
  [] -> throwM (SingleRowError {numberOfRowsReturned = 0})
  [one] -> pure one
  more ->
    throwM $
      SingleRowError
        { numberOfRowsReturned =
            -- TODO: this is VERY bad, because it requires to parse the full database output, even if there’s 10000000000 elements
            List.length more
        }

newtype Transaction m a = Transaction {unTransaction :: (ReaderT Connection m a)}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadThrow,
      MonadLogger,
      MonadIO,
      MonadUnliftIO,
      MonadTrans,
      Otel.MonadTracer
    )

runTransaction' :: Connection -> Transaction m a -> m a
runTransaction' conn transaction = runReaderT transaction.unTransaction conn

-- | [Resource Pool](http://hackage.haskell.org/package/resource-pool-0.2.3.2/docs/Data-Pool.html) configuration.
data PoolingInfo = PoolingInfo
  { -- | Minimal amount of resources that are
    --   always available.
    numberOfStripes :: AtLeast 1 Int,
    -- | Time after which extra resources
    --   (above minimum) can stay in the pool
    --   without being used.
    unusedResourceOpenTime :: Seconds,
    -- | Max number of resources that can be
    --   in the Pool at any time
    maxOpenResourcesPerStripe :: AtLeast 1 Int
  }
  deriving stock (Generic, Eq, Show)
  deriving anyclass (FromJSON)

initMonadPostgres ::
  (Text -> IO ()) ->
  -- | Info describing the connection to the Postgres DB
  Postgres.ConnectInfo ->
  -- | Configuration info for pooling attributes
  PoolingInfo ->
  -- | Created Postgres connection pool
  ResourceT IO (Pool Postgres.Connection)
initMonadPostgres logInfoFn connectInfo poolingInfo = do
  (_releaseKey, connPool) <-
    Resource.allocate
      (logInfoFn "Creating Postgres Connection Pool" >> createPGConnPool)
      (\pool -> logInfoFn "Destroying Postgres Connection Pool" >> destroyPGConnPool pool)
  pure connPool
  where
    -- \| Create a Postgres connection pool
    createPGConnPool ::
      IO (Pool Postgres.Connection)
    createPGConnPool =
      Pool.createPool
        poolCreateResource
        poolfreeResource
        poolingInfo.numberOfStripes.unAtLeast
        (poolingInfo.unusedResourceOpenTime & secondsToNominalDiffTime)
        (poolingInfo.maxOpenResourcesPerStripe.unAtLeast)
      where
        poolCreateResource = Postgres.connect connectInfo
        poolfreeResource = Postgres.close

    -- \| Destroy a Postgres connection pool
    destroyPGConnPool ::
      -- \| Pool to be destroyed
      (Pool Postgres.Connection) ->
      IO ()
    destroyPGConnPool p = Pool.destroyAllResources p

-- | Catch any Postgres exception that gets thrown,
-- print the query that was run and the query parameters,
-- then rethrow inside an 'Error'.
handlePGException ::
  forall a params tools m.
  (ToRow params, MonadUnliftIO m, MonadLogger m, HasField "pgFormat" tools Tool) =>
  tools ->
  Text ->
  Query ->
  -- | Depending on whether we used `format` or `formatMany`.
  Either params [params] ->
  IO a ->
  Transaction m a
handlePGException tools queryType query' params io = do
  withRunInIO $ \unliftIO ->
    io
      `catches` [ Handler $ unliftIO . logQueryException @SqlError,
                  Handler $ unliftIO . logQueryException @QueryError,
                  Handler $ unliftIO . logQueryException @ResultError,
                  Handler $ unliftIO . logFormatException
                ]
  where
    -- TODO: use throwInternalError here (after pulling it into the MonadPostgres class)
    throwAsError = unwrapIOError . Left . newError
    throwErr err = liftIO $ throwAsError $ prettyErrorTree $ nestedMultiError "A Postgres query failed" err
    logQueryException :: (Exception e) => e -> Transaction m a
    logQueryException exc = do
      formattedQuery <- case params of
        Left one -> pgFormatQuery' tools query' one
        Right many -> pgFormatQueryMany' tools query' many
      throwErr
        ( singleError [fmt|Query Type: {queryType}|]
            :| [ nestedError "Exception" (exc & showPretty & newError & singleError),
                 nestedError "Query" (formattedQuery & newError & singleError)
               ]
        )
    logFormatException :: FormatError -> Transaction m a
    logFormatException fe = throwErr (fe & showPretty & newError & singleError & singleton)

-- | Perform a Postgres action within a transaction
withPGTransaction ::
  -- | Postgres connection pool to be used for the action
  (Pool Postgres.Connection) ->
  -- | DB-action to be performed
  (Postgres.Connection -> IO a) ->
  -- | Result of the DB-action
  IO a
withPGTransaction connPool f =
  Pool.withResource
    connPool
    (\conn -> Postgres.withTransaction conn (f conn))

runPGTransactionImpl :: (MonadUnliftIO m) => m (Pool Postgres.Connection) -> Transaction m a -> m a
{-# INLINE runPGTransactionImpl #-}
runPGTransactionImpl zoom (Transaction transaction) = do
  pool <- zoom
  withRunInIO $ \unliftIO ->
    withPGTransaction pool $ \conn -> do
      unliftIO $ runReaderT transaction conn

executeImpl ::
  (ToRow params, MonadUnliftIO m, MonadLogger m, HasField "pgFormat" tools Tool, Otel.MonadTracer m) =>
  m tools ->
  m DebugLogDatabaseQueries ->
  Query ->
  params ->
  Transaction m (Label "numberOfRowsAffected" Natural)
{-# INLINE executeImpl #-}
executeImpl zoomTools zoomDebugLogDatabaseQueries qry params =
  Otel.inSpan' "Postgres Query (execute)" Otel.defaultSpanArguments $ \span -> do
    tools <- lift @Transaction zoomTools
    logDatabaseQueries <- lift @Transaction zoomDebugLogDatabaseQueries
    traceQueryIfEnabled tools span logDatabaseQueries qry (HasSingleParam params)
    conn <- Transaction ask
    PG.execute conn qry params
      & handlePGException tools "execute" qry (Left params)
      >>= toNumberOfRowsAffected "executeImpl"

executeImpl_ ::
  (MonadUnliftIO m, MonadLogger m, HasField "pgFormat" tools Tool, Otel.MonadTracer m) =>
  m tools ->
  m DebugLogDatabaseQueries ->
  Query ->
  Transaction m (Label "numberOfRowsAffected" Natural)
{-# INLINE executeImpl_ #-}
executeImpl_ zoomTools zoomDebugLogDatabaseQueries qry =
  Otel.inSpan' "Postgres Query (execute)" Otel.defaultSpanArguments $ \span -> do
    tools <- lift @Transaction zoomTools
    logDatabaseQueries <- lift @Transaction zoomDebugLogDatabaseQueries
    traceQueryIfEnabled @() tools span logDatabaseQueries qry HasNoParams
    conn <- Transaction ask
    PG.execute_ conn qry
      & handlePGException tools "execute_" qry (Left ())
      >>= toNumberOfRowsAffected "executeImpl_"

executeManyImpl ::
  (ToRow params, MonadUnliftIO m, MonadLogger m, HasField "pgFormat" tools Tool, Otel.MonadTracer m) =>
  m tools ->
  m DebugLogDatabaseQueries ->
  Query ->
  [params] ->
  Transaction m (Label "numberOfRowsAffected" Natural)
executeManyImpl zoomTools zoomDebugLogDatabaseQueries qry params =
  Otel.inSpan' "Postgres Query (execute)" Otel.defaultSpanArguments $ \span -> do
    tools <- lift @Transaction zoomTools
    logDatabaseQueries <- lift @Transaction zoomDebugLogDatabaseQueries
    traceQueryIfEnabled tools span logDatabaseQueries qry (HasMultiParams params)
    conn <- Transaction ask
    PG.executeMany conn qry params
      & handlePGException tools "executeMany" qry (Right params)
      >>= toNumberOfRowsAffected "executeManyImpl"

toNumberOfRowsAffected :: (MonadIO m) => Text -> Int64 -> m (Label "numberOfRowsAffected" Natural)
toNumberOfRowsAffected functionName i64 =
  i64
    & intToNatural
    & annotate [fmt|{functionName}: postgres returned a negative number of rows affected: {i64}|]
    -- we throw this directly in IO here, because we don’t want to e.g. have to propagate MonadThrow through user code (it’s an assertion)
    & unwrapIOError
    & liftIO
    <&> label @"numberOfRowsAffected"

executeManyReturningWithImpl ::
  (ToRow params, MonadUnliftIO m, MonadLogger m, HasField "pgFormat" tools Tool, Otel.MonadTracer m) =>
  m tools ->
  m DebugLogDatabaseQueries ->
  Query ->
  [params] ->
  Decoder r ->
  Transaction m [r]
{-# INLINE executeManyReturningWithImpl #-}
executeManyReturningWithImpl zoomTools zoomDebugLogDatabaseQueries qry params (Decoder fromRow) = do
  Otel.inSpan' "Postgres Query (execute)" Otel.defaultSpanArguments $ \span -> do
    tools <- lift @Transaction zoomTools
    logDatabaseQueries <- lift @Transaction zoomDebugLogDatabaseQueries
    traceQueryIfEnabled tools span logDatabaseQueries qry (HasMultiParams params)
    conn <- Transaction ask
    PG.returningWith fromRow conn qry params
      & handlePGException tools "executeManyReturning" qry (Right params)

foldRowsImpl ::
  (FromRow row, ToRow params, MonadUnliftIO m, MonadLogger m, HasField "pgFormat" tools Tool) =>
  m tools ->
  Query ->
  params ->
  a ->
  (a -> row -> Transaction m a) ->
  Transaction m a
{-# INLINE foldRowsImpl #-}
foldRowsImpl zoomTools qry params accumulator f = do
  conn <- Transaction ask
  tools <- lift @Transaction zoomTools
  withRunInIO
    ( \runInIO ->
        do
          PG.fold
            conn
            qry
            params
            accumulator
            (\acc row -> runInIO $ f acc row)
            & handlePGException tools "fold" qry (Left params)
            & runInIO
    )

pgFormatQueryNoParams' ::
  (MonadIO m, MonadLogger m, HasField "pgFormat" tools Tool) =>
  tools ->
  Query ->
  Transaction m Text
pgFormatQueryNoParams' tools q =
  lift $ pgFormatQueryByteString tools q.fromQuery

pgFormatQuery :: (ToRow params, MonadIO m) => Query -> params -> Transaction m ByteString
pgFormatQuery qry params = Transaction $ do
  conn <- ask
  liftIO $ PG.formatQuery conn qry params

pgFormatQueryMany :: (MonadIO m, ToRow params) => Query -> [params] -> Transaction m ByteString
pgFormatQueryMany qry params = Transaction $ do
  conn <- ask
  liftIO $ PG.formatMany conn qry params

queryWithImpl ::
  (ToRow params, MonadUnliftIO m, MonadLogger m, HasField "pgFormat" tools Tool, Otel.MonadTracer m) =>
  m tools ->
  m DebugLogDatabaseQueries ->
  Query ->
  params ->
  Decoder r ->
  Transaction m [r]
{-# INLINE queryWithImpl #-}
queryWithImpl zoomTools zoomDebugLogDatabaseQueries qry params (Decoder fromRow) = do
  Otel.inSpan' "Postgres Query (execute)" Otel.defaultSpanArguments $ \span -> do
    tools <- lift @Transaction zoomTools
    logDatabaseQueries <- lift @Transaction zoomDebugLogDatabaseQueries
    traceQueryIfEnabled tools span logDatabaseQueries qry (HasSingleParam params)
    conn <- Transaction ask
    PG.queryWith fromRow conn qry params
      & handlePGException tools "query" qry (Left params)

queryWithImpl_ :: (MonadUnliftIO m, MonadLogger m, HasField "pgFormat" tools Tool) => m tools -> Query -> Decoder r -> Transaction m [r]
{-# INLINE queryWithImpl_ #-}
queryWithImpl_ zoomTools qry (Decoder fromRow) = do
  tools <- lift @Transaction zoomTools
  conn <- Transaction ask
  liftIO (PG.queryWith_ fromRow conn qry)
    & handlePGException tools "query" qry (Left ())

pgQuery :: (ToRow params, FromRow r, MonadUnliftIO m, MonadLogger m, HasField "pgFormat" tools Tool) => tools -> Query -> params -> Transaction m [r]
pgQuery tools qry params = do
  conn <- Transaction ask
  PG.query conn qry params
    & handlePGException tools "query" qry (Left params)

pgQuery_ :: (FromRow r, MonadUnliftIO m, MonadLogger m, HasField "pgFormat" tools Tool) => tools -> Query -> Transaction m [r]
pgQuery_ tools qry = do
  conn <- Transaction ask
  PG.query_ conn qry
    & handlePGException tools "query_" qry (Left ())

data SingleRowError = SingleRowError
  { -- | How many columns were actually returned by the query
    numberOfRowsReturned :: Int
  }
  deriving stock (Show)

instance Exception SingleRowError where
  displayException (SingleRowError {..}) = [fmt|Single row expected from SQL query result, {numberOfRowsReturned} rows were returned instead."|]

pgFormatQuery' :: (MonadIO m, ToRow params, MonadLogger m, HasField "pgFormat" tools Tool) => tools -> Query -> params -> Transaction m Text
pgFormatQuery' tools q p =
  pgFormatQuery q p
    >>= lift . pgFormatQueryByteString tools

pgFormatQueryMany' :: (MonadIO m, ToRow params, MonadLogger m, HasField "pgFormat" tools Tool) => tools -> Query -> [params] -> Transaction m Text
pgFormatQueryMany' tools q p =
  pgFormatQueryMany q p
    >>= lift . pgFormatQueryByteString tools

-- | Read the executable name "pg_format"
postgresToolsParser :: ToolParserT IO (Label "pgFormat" Tool)
postgresToolsParser = label @"pgFormat" <$> readTool "pg_format"

pgFormatQueryByteString :: (MonadIO m, MonadLogger m, HasField "pgFormat" tools Tool) => tools -> ByteString -> m Text
pgFormatQueryByteString tools queryBytes = do
  do
    (exitCode, stdout, stderr) <-
      Process.readProcessWithExitCode
        tools.pgFormat.toolPath
        ["-"]
        (queryBytes & bytesToTextUtf8Lenient & textToString)
    case exitCode of
      ExitSuccess -> pure (stdout & stringToText)
      ExitFailure status -> do
        $logWarn [fmt|pg_format failed with status {status} while formatting the query, using original query string. Is there a syntax error?|]
        $logDebug
          ( prettyErrorTree
              ( nestedMultiError
                  "pg_format output"
                  ( nestedError "stdout" (singleError (stdout & stringToText & newError))
                      :| [(nestedError "stderr" (singleError (stderr & stringToText & newError)))]
                  )
              )
          )
        $logDebug [fmt|pg_format stdout: stderr|]
        pure (queryBytes & bytesToTextUtf8Lenient)

data DebugLogDatabaseQueries
  = -- | Do not log the database queries
    DontLogDatabaseQueries
  | -- | Log the database queries as debug output;
    LogDatabaseQueries
  | -- | Log the database queries as debug output and additionally the EXPLAIN output (from the query analyzer, not the actual values after execution cause that’s a bit harder to do)
    LogDatabaseQueriesAndExplain
  deriving stock (Show, Enum, Bounded)

data HasQueryParams param
  = HasNoParams
  | HasSingleParam param
  | HasMultiParams [param]

-- | Log the postgres query depending on the given setting
traceQueryIfEnabled ::
  ( ToRow params,
    MonadUnliftIO m,
    MonadLogger m,
    HasField "pgFormat" tools Tool,
    Otel.MonadTracer m
  ) =>
  tools ->
  Otel.Span ->
  DebugLogDatabaseQueries ->
  Query ->
  HasQueryParams params ->
  Transaction m ()
traceQueryIfEnabled tools span logDatabaseQueries qry params = do
  -- In case we have query logging enabled, we want to do that
  let formattedQuery = case params of
        HasNoParams -> pgFormatQueryNoParams' tools qry
        HasSingleParam p -> pgFormatQuery' tools qry p
        HasMultiParams ps -> pgFormatQueryMany' tools qry ps
  let doLog errs =
        Otel.addAttributes
          span
          $ HashMap.fromList
          $ ( ("postgres.query", Otel.toAttribute @Text errs.query)
                : ( errs.explain
                      & foldMap
                        ( \ex ->
                            [("postgres.explain", Otel.toAttribute @Text ex)]
                        )
                  )
            )
  let doExplain = do
        q <- formattedQuery
        Otel.inSpan "Postgres EXPLAIN Query" Otel.defaultSpanArguments $ do
          queryWithImpl_
            (pure tools)
            ( "EXPLAIN "
                <> (
                     -- TODO: this is not nice, but the only way to get the `executeMany` form to work with this
                     -- because we need the query with all elements already interpolated.
                     Query (q & textToBytesUtf8)
                   )
            )
            (Dec.fromField @Text)
            <&> Text.intercalate "\n"
  case logDatabaseQueries of
    DontLogDatabaseQueries -> pure ()
    LogDatabaseQueries -> do
      q <- formattedQuery
      doLog (T2 (label @"query" q) (label @"explain" Nothing))
    LogDatabaseQueriesAndExplain -> do
      q <- formattedQuery
      -- XXX: stuff like `CREATE SCHEMA` cannot be EXPLAINed, so we should catch exceptions here
      -- and just ignore anything that errors (if it errors because of a problem with the query, it would have been caught by the query itself.
      ex <- doExplain
      doLog (T2 (label @"query" q) (label @"explain" (Just ex)))

instance (ToField t1) => ToRow (Label l1 t1) where
  toRow t2 = toRow $ PG.Only $ getField @l1 t2

instance (ToField t1, ToField t2) => ToRow (T2 l1 t1 l2 t2) where
  toRow t2 = toRow (getField @l1 t2, getField @l2 t2)

instance (ToField t1, ToField t2, ToField t3) => ToRow (T3 l1 t1 l2 t2 l3 t3) where
  toRow t3 = toRow (getField @l1 t3, getField @l2 t3, getField @l3 t3)
