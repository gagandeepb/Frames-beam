-- | This module provides certain "canned" queries that would be useful for
-- pulling Postgres rows into a data frame. Also contains a utility function
-- for handling results of join queries.
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

type PostgresTable a b =
  DatabaseSettings Postgres b -> DatabaseEntity Postgres b (TableEntity a)

type PostgresDB b = DatabaseSettings Postgres b

type PostgresFilterLambda a s =
  (a (QExpr PgExpressionSyntax s)) -> QExpr PgExpressionSyntax s Bool

type JoinQueryResults a b = [(a Identity, b Identity)]

-- | Helps select all rows from a particular table in a database.
-- Note that the table and database declaration is present in
-- the module generated by @genBeamSchema@, invoked in the end-user code.
-- Equivalent to SQL: "SELECT * FROM tbl;" when run using an appropriate
-- run function from the "FramesBeam.Streaming" module.
allRows :: (Database Postgres b, Table a) =>
            PostgresTable a b
            -> PostgresDB b
            -> Q PgSelectSyntax b s (a (QExpr PgExpressionSyntax s))
allRows tbl db = all_ (tbl db)

-- | Helps select all rows from a particular table in a database that
-- satisfy a certain filter condition that is executed at the DB-level.
-- Note that the table and database declaration is present in
-- the module generated by @genBeamSchema@, invoked in the end-user code.
-- Equivalent to SQL: "SELECT * FROM tbl WHERE ...;" when run using an
-- appropriate run function from the "FramesBeam.Streaming" module.
allRowsWhere :: (Database Postgres b, Table a) =>
                PostgresTable a b
                -> PostgresDB b
                -> PostgresFilterLambda a s
                -> Q PgSelectSyntax b s (a (QExpr PgExpressionSyntax s))
allRowsWhere tbl db filterLambda =
  filter_ (filterLambda) (allRows tbl db)

-- | Function for creating a data frame from the results of executing
-- a join query.
join2 :: (Table a, Table b, GenericVinyl (a Identity) a_names a_rs,
          GenericVinyl (b Identity) b_names b_rs, RecVec (ZipTypes a_names a_rs),
          RecVec (ZipTypes b_names b_rs)) =>
          JoinQueryResults a b ->
          FrameRec (ZipTypes a_names a_rs ++ ZipTypes b_names b_rs)
join2 joinQueryResults =
  zipFrames aFrame bFrame
  where
    (aQRes, bQRes) = unzip joinQueryResults
    aRecs = map createRecId aQRes
    bRecs = map createRecId bQRes
    aFrame = toFrame aRecs
    bFrame = toFrame bRecs


