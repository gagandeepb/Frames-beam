{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}
module UsageExample where
-- Usage Example
import           Data.Coerce
import qualified Data.Vinyl.Functor            as VF
import           Database.Beam
import           Database.Beam.Postgres
import           Database.Beam.Postgres.Syntax
import qualified Frames                        as F
import           FramesBeam.MigrationAutogen
import           FramesBeam.Vinylize           (createRecId, deriveVinyl)
import           Generics.SOP
import           Generics.SOP.TH
-- Comment out the import of `Schema` when compiling this module for the first time;
-- this would allow the Schema module to be generated on the first compilation
-- Once `Schema` is generated (or rather "bootstrapped"),
-- there is no need of commenting out this import
import           Schema

-- generates a Beam schema file in src/ directory, named as `Schema.hs`
$(genBeamSchema "host=localhost dbname=shoppingcart1")

deriveGeneric ''Cart_usersT

deriveVinyl ''Cart_usersT

allUsers :: Q PgSelectSyntax Db s (Cart_usersT (QExpr PgExpressionSyntax s))
allUsers = all_ (_cart_users db)

selectAllUsers :: Connection -> IO [Cart_usersT Identity]
selectAllUsers conn =
  runBeamPostgresDebug putStrLn conn $ do
    users <- runSelectReturningList $ select allUsers
    (liftIO . return) users

test :: IO ()
test = do
  conn <- connectPostgreSQL "host=localhost dbname=shoppingcart1"
  us <- selectAllUsers conn
  mapM_ print $ F.toFrame $ map createRecId us

