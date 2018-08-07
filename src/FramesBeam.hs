-- | Re-exports the underlying modules/libraries.
module FramesBeam
    (
      -- * Re-exports the underlying modules
      module FramesBeam.Vinylize
    , module FramesBeam.BeamSchemaGen
    , module FramesBeam.Query
    , module FramesBeam.Streaming

      -- * Re-exports @beam-core@ and @beam-postgres@
    , module Database.Beam
    , module Database.Beam.Postgres

      -- * Postgres Column Types
    , ByteString
    , Text
    , UUID
    , Scientific
    , UTCTime
    , LocalTimestamp
    , UTCTimestamp
    , ZonedTimestamp
    , LocalTime
    , TimeOfDay
    , Date
    , Day

      -- * Re-exports for the @deriveGeneric@ plus @deriveVinyl@ combination
    , module Data.Coerce
    , module Generics.SOP
    , deriveGeneric

    ) where

import           FramesBeam.BeamSchemaGen
import           FramesBeam.Query
import           FramesBeam.Streaming
import           FramesBeam.Vinylize


import           Database.Beam
import           Database.Beam.Postgres

import           Data.ByteString                 (ByteString)
import           Data.Scientific                 (Scientific)
import           Data.Text                       (Text)
import           Data.Time                       (Day, LocalTime, TimeOfDay,
                                                  UTCTime)
import           Data.UUID.Types                 (UUID)
import           Database.PostgreSQL.Simple.Time (Date, LocalTimestamp,
                                                  UTCTimestamp, ZonedTimestamp)

import           Data.Coerce
import           Generics.SOP                    hiding (Generic, fieldName)
import           Generics.SOP.TH                 (deriveGeneric)

