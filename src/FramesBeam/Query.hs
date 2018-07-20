{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds        #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}
module FramesBeam.Query where

import           Data.Vinyl.TypeLevel
import           Database.Beam
import           Database.Beam.Postgres
import           Database.Beam.Postgres.Syntax
import           Frames.Frame
import           Frames.InCore
import           FramesBeam.Vinylize

allRows :: (Database Postgres b, Table a) =>
              (DatabaseSettings Postgres b -> DatabaseEntity Postgres b (TableEntity a)) ->
              DatabaseSettings Postgres b ->
              Q PgSelectSyntax b s (a (QExpr PgExpressionSyntax s))
allRows tbl db = all_ (tbl db)


allRowsWhere :: (Database Postgres b, Table a) =>
                        (DatabaseSettings Postgres b -> DatabaseEntity Postgres b (TableEntity a)) ->
                        DatabaseSettings Postgres b ->
                        ((a (QExpr PgExpressionSyntax s)) -> QExpr PgExpressionSyntax s Bool)  ->
                        Q PgSelectSyntax b s (a (QExpr PgExpressionSyntax s))
allRowsWhere tbl db filterLambda =
  filter_ (filterLambda) (allRows tbl db)


join2 :: (Table a, Table b, GenericVinyl (a Identity) a_names a_rs,
          GenericVinyl (b Identity) b_names b_rs, RecVec (ZipTypes a_names a_rs),
          RecVec (ZipTypes b_names b_rs)) =>
          [(a Identity, b Identity)] ->
          FrameRec (ZipTypes a_names a_rs ++ ZipTypes b_names b_rs)
join2 joinQueryResults =
  zipFrames aFrame bFrame
  where
    (aQRes, bQRes) = unzip joinQueryResults
    aRecs = map createRecId aQRes
    bRecs = map createRecId bQRes
    aFrame = toFrame aRecs
    bFrame = toFrame bRecs


