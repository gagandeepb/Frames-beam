-- | Template Haskell helper functions used internally.
{-# LANGUAGE TemplateHaskell #-}
module FramesBeam.Helpers (
  fNamesTypeLevel
) where

import           Language.Haskell.TH

-- | Returns a type-level list of record field names
fNamesTypeLevel :: Name -> Q Type
fNamesTypeLevel name = do
  fnames <- fmap getRecordFields $ reify name
  fnames' <- fnames
  foldr (\x xs -> appT (appT promotedConsT x) xs) promotedNilT $ map (litT . strTyLit) fnames'


getRecordFields :: Info -> Q [String]
getRecordFields (TyConI (DataD _ _ _ _ cons _)) = return $ concatMap getRF cons
getRecordFields _                               = return []

getRF :: Con -> [String]
getRF (RecC _name fields) = map getFieldInfo fields
getRF _                   = []

getFieldInfo :: (Name, Strict, Type) -> String
getFieldInfo (name, _, AppT (AppT (ConT _) (VarT _f)) (ConT _ty)) = (nameBase name)
getFieldInfo (_, _, _) = error "Inappropriate name passed"






