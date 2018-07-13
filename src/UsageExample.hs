{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
module UsageExample where
-- Usage Example


import           Data.Coerce
import           Data.Conduit                   (ConduitT, Void, await,
                                                 runConduit, yield, yieldM,
                                                 (.|), (=$=))
import qualified Data.Conduit.Combinators       as CC
import qualified Data.Conduit.List              as CL (mapM_, sourceList, take)
import           Data.String                    (fromString)
import qualified Data.Vinyl.Functor             as VF
import           Database.Beam
import           Database.Beam.Postgres
import qualified Database.Beam.Postgres.Conduit as DBPC
import           Database.Beam.Postgres.Syntax
import qualified Database.PostgreSQL.Simple     as Pg
import qualified Frames                         as F
import           FramesBeam.MigrationAutogen
import           FramesBeam.Vinylize            (createRecId, deriveVinyl)
import           Generics.SOP
import           Generics.SOP.TH
-- Comment out the import of `Schema` when compiling this module for the first time;
-- this would allow the Schema module to be generated on the first compilation
-- Once `Schema` is generated (or rather "bootstrapped"),
-- there is no need of commenting out this import
import           Schema


-- generates a Beam schema file in src/ directory, named as `Schema.hs`
-- $(genBeamSchema "host=localhost dbname=shoppingcart1")

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
  conn <- connectPostgreSQL  "host=localhost dbname=shoppingcart1"
  us <- selectAllUsers conn
  mapM_ print $ F.toFrame $ map createRecId us



-- Streaming

-- Question: Is this going to execute lazily in testStream2 ?
source ::  Connection -> ConduitT () (Cart_usersT Identity) IO ()
source conn = do
  us <- liftIO $ selectAllUsers conn
  CL.sourceList $ us

conduit :: ConduitT (Cart_usersT Identity) String IO ()
conduit = do
  row <- await
  -- liftIO $ print val
  case row of
    Nothing -> return ()
    Just r -> do
      yield $ show (createRecId r)
      conduit

sink :: Int -> ConduitT String Void IO [String]
sink n = do
  CL.take n

conduit2 :: ConduitT (Cart_usersT Identity) (F.Record Fs) IO ()
conduit2 = do
  row <- await
  -- liftIO $ print val
  case row of
    Nothing -> return ()
    Just r -> do
      yield $ (createRecId r)
      conduit2

sink2 :: Int -> ConduitT (F.Record Fs) Void IO [F.Record Fs]
sink2 n = do
  CL.take n

testStream :: Int -> IO ()
testStream n = do
  conn <- connectPostgreSQL  "host=localhost dbname=shoppingcart1"
  res <- runConduit $ (source conn)  .| conduit .| (sink n)
  mapM_ putStrLn res

-- Question: Is this going to execute lazily?
testStream2 :: Int -> IO ()
testStream2 n = do
  conn <- connectPostgreSQL  "host=localhost dbname=shoppingcart1"
  res <- runConduit $ (source conn)  .| conduit2 .| (sink2 n)
  mapM_ print (F.toFrame res)

type Fs = '["_cart_usersEmail" F.:-> F.Text,
            "_cart_usersFirst_name" F.:-> F.Text,
            "_cart_usersLast_name" F.:-> F.Text,
            "_cart_usersIs_member" F.:-> Bool,
            "_cart_usersDays_in_queue" F.:-> Int]
