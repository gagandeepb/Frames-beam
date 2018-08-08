-- | Typeclass and an instance generator for the typeclass to convert
-- plain Haskell records to their vinyl representation.
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
module Frames.SQL.Beam.Postgres.Vinylize where

import           Data.Proxy
import           Data.Vinyl
import qualified Data.Vinyl.Functor               as VF
import qualified Database.Beam                    as B
import           Frames.Col
import           Frames.SQL.Beam.Postgres.Helpers (fNamesTypeLevel)
import           Generics.SOP
import qualified Generics.SOP.NP                  as GSN
import           GHC.TypeLits
import           Language.Haskell.TH

-- | Type family that generates the column types for the vinyl representation.
type family ZipTypes (ns :: [Symbol])  (ys :: [*]) = (zs :: [k]) | zs -> ns ys
type instance ZipTypes '[] '[] = '[]
type instance ZipTypes (n ': ns) (y ': ys)  =  ( n  :-> y) ': (ZipTypes ns  ys)

-- | Typeclass for converting a plain Haskell record to it's vinyl
-- representation.
class GenericVinyl a names rs | a -> names rs where
  type FieldNames a :: [Symbol]
  createRecId :: a  -> Rec VF.Identity (ZipTypes names rs)

-- | Helps generate an instance for @GenericVinyl@, given a plain
-- Haskell record declaration name. Uses Template Haskell, so
-- if, say, the record is named @MyRecord@, then first you must
-- invoke @deriveGeneric ''MyRecord@ to get the Sum-of-Products (SOP)
-- representation (imported from @generic-sop@) of the record in-scope,
-- in the current module. This is followed by invoking
-- @deriveVinyl ''MyRecord@, which makes use of the SOP representation
-- of the plain record and generates a @GenericVinyl@ instance for the record.
deriveVinyl :: Name -> DecsQ
deriveVinyl name = entireInstance
  where
    n = conT name
    typeList1 = fNamesTypeLevel name
    entireInstance=
      [d|
        instance (((Code ($(n) B.Identity)) ~ '[rs]),
          (ns3 ~ FieldNames ($(n) B.Identity)) )
          => GenericVinyl ($(n) B.Identity) ns3 rs where
          type FieldNames ($(n) B.Identity) = $(typeList1)
          createRecId r = (go tranformedNP)
            where
              SOP (Z prod) = from r
              tranformedNP = (((GSN.trans_NP (Proxy :: Proxy (LiftedCoercible I VF.Identity)) (\(I x) -> VF.Identity $ coerce x) prod)) )
              go = GSN.cata_NP RNil (:&)
         |]
