{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
module FramesBeam.Streaming where

import           Control.Exception              (bracket)
import           Control.Monad.Trans.Control    (MonadBaseControl)
import qualified Data.ByteString                as B
import           Data.Conduit                   (ConduitT, runConduit, (.|))
import qualified Data.Conduit.List              as CL
import           Database.Beam
import           Database.Beam.Postgres
import qualified Database.Beam.Postgres.Conduit as DBPC
import           Database.Beam.Postgres.Syntax
import qualified Database.PostgreSQL.Simple     as Pg
import           Frames.Rec                     (Record)
import           FramesBeam.Query
import           FramesBeam.Vinylize

bulkSelectAllRows ::
  (Database Postgres b, Table a, MonadIO m,
    MonadBaseControl IO m,
    FromBackendRow Postgres (a Identity)) =>
    (DatabaseSettings Postgres b ->
      DatabaseEntity Postgres b (TableEntity a)) ->
    DatabaseSettings Postgres b ->
    Int ->
    Connection ->
    m [(a Identity)]
bulkSelectAllRows tbl db nrows conn =
  DBPC.runSelect conn (select (allRows tbl db)) (\c -> runConduit $ c .| CL.take nrows)


bulkSelectAllRowsWhere ::
  (Database Postgres b, Table a, MonadIO m,
    MonadBaseControl IO m,
    FromBackendRow Postgres (a Identity)) =>
      (DatabaseSettings Postgres b ->
        DatabaseEntity Postgres b (TableEntity a)) ->
      DatabaseSettings Postgres b ->
      Int ->
      (forall s. (a (QExpr PgExpressionSyntax s)) ->
        QExpr PgExpressionSyntax s Bool) ->
      Connection ->
      m [(a Identity)]
bulkSelectAllRowsWhere tbl db nrows filterLambda conn =
  DBPC.runSelect conn (select (allRowsWhere tbl db filterLambda)) (\c -> runConduit $ c .| CL.take nrows)

streamingSelectAllPipeline ::
  (Database Postgres b, Table a, MonadIO m, MonadBaseControl IO m,
    FromBackendRow Postgres (a Identity),
    GenericVinyl (a Identity) a_names a_rs) =>
    (DatabaseSettings Postgres b ->
      DatabaseEntity Postgres b (TableEntity a)) ->
    DatabaseSettings Postgres b ->
    Int ->
    ConduitT (Record (ZipTypes a_names a_rs)) out m () ->
    Connection ->
    m [out]
streamingSelectAllPipeline tbl db nrows recordProcessorConduit conn =
  DBPC.runSelect conn (select (allRows tbl db)) $
    (\c -> runConduit $ c .| CL.map createRecId
                          .| recordProcessorConduit
                          .| CL.take nrows)

streamingSelectAllPipeline' ::
  (Database Postgres b, Table a, MonadIO m, MonadBaseControl IO m,
    FromBackendRow Postgres (a Identity),
    GenericVinyl (a Identity) a_names a_rs) =>
      (DatabaseSettings Postgres b ->
        DatabaseEntity Postgres b (TableEntity a)) ->
      DatabaseSettings Postgres b ->
      Int ->
      (forall s. (a (QExpr PgExpressionSyntax s)) ->
        QExpr PgExpressionSyntax s Bool) ->
      ConduitT (Record (ZipTypes a_names a_rs)) out m () ->
      Connection ->
      m [out]
streamingSelectAllPipeline' tbl db nrows filterLambda recordProcessorConduit conn =
  DBPC.runSelect conn (select (allRowsWhere tbl db filterLambda)) $
    (\c -> runConduit $ c .| CL.map createRecId
                          .| recordProcessorConduit
                          .| CL.take nrows)

withConnection :: B.ByteString -> (Connection -> IO a) -> IO a
withConnection connString =
  bracket
    (connectPostgreSQL connString)
    (Pg.close)
