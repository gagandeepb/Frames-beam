-- | Functions for streaming DB queries.
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
module Frames.SQL.Beam.Postgres.Streaming where

import           Control.Exception                 (bracket)
import           Control.Monad.Trans.Control       (MonadBaseControl)
import qualified Data.ByteString                   as B
import           Data.Conduit                      (ConduitT, runConduit, (.|))
import qualified Data.Conduit.List                 as CL
import           Database.Beam
import           Database.Beam.Postgres
import qualified Database.Beam.Postgres.Conduit    as DBPC
import           Database.Beam.Postgres.Syntax
import qualified Database.PostgreSQL.Simple        as Pg
import           Frames.Rec                        (Record)
import           Frames.SQL.Beam.Postgres.Query
import           Frames.SQL.Beam.Postgres.Vinylize

type NumberOfRows = Int

-- | Selects a given number of rows. Result returned as a list of plain
-- Haskell records.
bulkSelectAllRows ::
  (Database Postgres b, Table a, MonadIO m,
    MonadBaseControl IO m,
    FromBackendRow Postgres (a Identity)) =>
    PostgresTable a b
    -> PostgresDB b
    -> NumberOfRows
    -> Connection
    -> m [(a Identity)]
bulkSelectAllRows tbl db nrows conn =
  DBPC.runSelect conn (select (allRows tbl db)) (\c -> runConduit $ c .| CL.take nrows)

-- | Selects a given number of rows that satisfy a filter condition that
-- is executed at the DB-level. Result returned as a list of plain
-- Haskell records.
bulkSelectAllRowsWhere ::
  (Database Postgres b, Table a, MonadIO m,
    MonadBaseControl IO m,
    FromBackendRow Postgres (a Identity)) =>
      PostgresTable a b
      -> PostgresDB b
      -> NumberOfRows
      -> (forall s. PostgresFilterLambda a s)
      -> Connection
      -> m [(a Identity)]
bulkSelectAllRowsWhere tbl db nrows filterLambda conn =
  DBPC.runSelect conn (select (allRowsWhere tbl db filterLambda)) (\c -> runConduit $ c .| CL.take nrows)

-- | Selects a given number of rows, and processes them using a user-provided
-- @conduit@. User-provided @conduit@ takes in vinyl-records, and sends output
-- downstream. Result returned as a list of outputs of the @conduit@.
streamingSelectAllPipeline ::
  (Database Postgres b, Table a, MonadIO m, MonadBaseControl IO m,
    FromBackendRow Postgres (a Identity),
    GenericVinyl (a Identity) a_names a_rs) =>
    PostgresTable a b
    -> PostgresDB b
    -> NumberOfRows
    -> ConduitT (Record (ZipTypes a_names a_rs)) out m ()
    -> Connection
    -> m [out]
streamingSelectAllPipeline tbl db nrows recordProcessorConduit conn =
  DBPC.runSelect conn (select (allRows tbl db)) $
    (\c -> runConduit $ c .| CL.map createRecId
                          .| recordProcessorConduit
                          .| CL.take nrows)

-- | Similar as @streamingSelectAllPipeline@, with an additional @filterLambda@
-- parameter that executes a SQL 'SELECT * FROM tbl WHERE' at the DB-level
-- prior to sending results downstream.
streamingSelectAllPipeline' ::
  (Database Postgres b, Table a, MonadIO m, MonadBaseControl IO m,
    FromBackendRow Postgres (a Identity),
    GenericVinyl (a Identity) a_names a_rs) =>
    PostgresTable a b
    -> PostgresDB b
    -> NumberOfRows
    -> (forall s. PostgresFilterLambda a s)
    -> ConduitT (Record (ZipTypes a_names a_rs)) out m ()
    -> Connection
    -> m [out]
streamingSelectAllPipeline' tbl db nrows filterLambda recordProcessorConduit conn =
  DBPC.runSelect conn (select (allRowsWhere tbl db filterLambda)) $
    (\c -> runConduit $ c .| CL.map createRecId
                          .| recordProcessorConduit
                          .| CL.take nrows)

-- | Takes in the connection string, one of the the other 4 functions in
-- in this module with just the @Connection@ object unapplied. Internally,
-- it acquires the connection object using the string, passes it to the
-- function that performs an IO action, and then cleans up afterwards by
-- closing the database connection.
withConnection :: B.ByteString -> (Connection -> IO a) -> IO a
withConnection connString =
  bracket
    (connectPostgreSQL connString)
    (Pg.close)
