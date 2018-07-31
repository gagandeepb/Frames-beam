{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
module FramesBeam.Streaming where

import           Control.Monad.Trans.Control    (MonadBaseControl)
import           Data.Conduit                   (ConduitT, runConduit, (.|))
import qualified Data.Conduit.List              as CL
import           Database.Beam
import           Database.Beam.Postgres
import qualified Database.Beam.Postgres.Conduit as DBPC
import           Database.Beam.Postgres.Syntax
import           Frames.Rec                     (Record)
import           FramesBeam.Query
import           FramesBeam.Vinylize


bulkSelectAllRows ::
  (Database Postgres b, Table a, MonadIO m,
    MonadBaseControl IO m,
    FromBackendRow Postgres (a Identity)) =>
    Connection ->
    (DatabaseSettings Postgres b ->
      DatabaseEntity Postgres b (TableEntity a)) ->
    DatabaseSettings Postgres b ->
    Int ->
    m [(a Identity)]
bulkSelectAllRows conn tbl db nrows =
  DBPC.runSelect conn (select (allRows tbl db)) (\c -> runConduit $ c .| CL.take nrows)


bulkSelectAllRowsWhere ::
  (Database Postgres b, Table a, MonadIO m,
    MonadBaseControl IO m,
    FromBackendRow Postgres (a Identity)) =>
      Connection ->
      (DatabaseSettings Postgres b ->
        DatabaseEntity Postgres b (TableEntity a)) ->
      DatabaseSettings Postgres b ->
      Int ->
      (forall s. (a (QExpr PgExpressionSyntax s)) ->
        QExpr PgExpressionSyntax s Bool) ->
      m [(a Identity)]
bulkSelectAllRowsWhere conn tbl db nrows filterLambda =
  DBPC.runSelect conn (select (allRowsWhere tbl db filterLambda)) (\c -> runConduit $ c .| CL.take nrows)

streamingSelectAllPipeline ::
  (Database Postgres b, Table a, MonadIO m, MonadBaseControl IO m,
    FromBackendRow Postgres (a Identity),
    GenericVinyl (a Identity) a_names a_rs) =>
    Connection ->
    (DatabaseSettings Postgres b ->
      DatabaseEntity Postgres b (TableEntity a)) ->
    DatabaseSettings Postgres b ->
    Int ->
    ConduitT (Record (ZipTypes a_names a_rs)) out m () ->
    m [out]
streamingSelectAllPipeline conn tbl db nrows recordProcessorConduit =
  DBPC.runSelect conn (select (allRows tbl db)) $
    (\c -> runConduit $ c .| CL.map createRecId
                          .| recordProcessorConduit
                          .| CL.take nrows)

streamingSelectAllPipeline' ::
  (Database Postgres b, Table a, MonadIO m, MonadBaseControl IO m,
    FromBackendRow Postgres (a Identity),
    GenericVinyl (a Identity) a_names a_rs) =>
      Connection ->
      (DatabaseSettings Postgres b ->
        DatabaseEntity Postgres b (TableEntity a)) ->
      DatabaseSettings Postgres b ->
      Int ->
      (forall s. (a (QExpr PgExpressionSyntax s)) ->
        QExpr PgExpressionSyntax s Bool) ->
      ConduitT (Record (ZipTypes a_names a_rs)) out m () ->
      m [out]
streamingSelectAllPipeline' conn tbl db nrows filterLambda recordProcessorConduit =
  DBPC.runSelect conn (select (allRowsWhere tbl db filterLambda)) $
    (\c -> runConduit $ c .| CL.map createRecId
                          .| recordProcessorConduit
                          .| CL.take nrows)





