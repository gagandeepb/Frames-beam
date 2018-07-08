{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
module FramesBeam.Vinylize where

import           Data.Proxy
import           Data.Vinyl
import qualified Data.Vinyl.Functor  as VF
import qualified Database.Beam       as B
import           Frames.Col
import           FramesBeam.Helpers  (fNamesTypeLevel)
import           Generics.SOP
import qualified Generics.SOP.NP     as GSN
import           GHC.TypeLits
import           Language.Haskell.TH


type family ZipTypes (ns :: [Symbol])  (ys :: [*]) = (zs :: [k]) | zs -> ns ys
type instance ZipTypes '[] '[] = '[]
type instance ZipTypes (n ': ns) (y ': ys)  =  ( n  :-> y) ': (ZipTypes ns  ys)


class GenericVinyl a names rs | a -> names rs where
  type FieldNames a :: [Symbol]
  createRecId :: a  -> Rec VF.Identity (ZipTypes names rs)

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
