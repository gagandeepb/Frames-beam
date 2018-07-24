{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
module UsageExample where
-- Usage Example


import           Data.Coerce
import           Data.Conduit                   (runConduit, (.|))
import qualified Data.Conduit.List              as CL
import           Data.Text                      (Text)
import qualified Data.Vinyl.Functor             as VF
import           Database.Beam
import           Database.Beam.Postgres
import qualified Database.Beam.Postgres.Conduit as DBPC
import qualified Frames                         as F
import           FramesBeam.MigrationAutogen
import           FramesBeam.Query
import           FramesBeam.Streaming
import           FramesBeam.Vinylize            (createRecId, deriveVinyl)
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

selectAllUsers :: Connection -> IO [Cart_usersT Identity]
selectAllUsers conn =
  runBeamPostgresDebug putStrLn conn $ do
    users <- runSelectReturningList $ select (allRows _cart_users db)
    (liftIO . return) users

test :: IO ()
test = do
  conn <- connectPostgreSQL  "host=localhost dbname=shoppingcart1"
  us <- selectAllUsers conn
  mapM_ print $ F.toFrame $ map createRecId us



-- Streaming
testStream1 :: Int -> IO [(Cart_usersT Identity)]
testStream1 n = do
  conn <- connectPostgreSQL  "host=localhost dbname=shoppingcart1"
  res <- DBPC.runSelect conn (select (allRows _cart_users db)) (\c -> runConduit $ c .| CL.take n)
  return res

testStream3 :: Int -> IO ()
testStream3 n = do
  conn <- connectPostgreSQL  "host=localhost dbname=shoppingcart1"
  res <- DBPC.runSelect conn (select (allRowsWhere _cart_users db (\c -> (_cart_usersFirst_name c) `like_` "J%") )) (\c -> runConduit $ c .| CL.take n)
  mapM_ print $ map createRecId  res


testStream4 :: Int -> IO [(Cart_usersT Identity)]
testStream4 n = do
  conn <- connectPostgreSQL  "host=localhost dbname=shoppingcart1"
  res <- DBPC.runSelect conn (select (allRowsWhere _cart_users db (\c -> (_cart_usersFirst_name c) `like_` "J%") )) (\c -> runConduit $ c .| CL.take n)
  return res


testStream5 :: IO ()
testStream5 = do
  conn <- connectPostgreSQL  "host=localhost dbname=shoppingcart1"
  res <- bulkSelectAllRows conn _cart_users db 1000
  mapM_ (print . createRecId) res


testStream6 :: IO ()
testStream6 = do
  conn <- connectPostgreSQL  "host=localhost dbname=shoppingcart1"
  res <- streamingSelectAllPipeline' conn _cart_users db 1000 (\c -> (_cart_usersFirst_name c) `like_` "J%") (CL.isolate 1000)
  mapM_ print res

-- Streaming column-subset
testStream7 :: IO ()
testStream7 = do
  conn <- connectPostgreSQL  "host=localhost dbname=shoppingcart1"
  res <- streamingSelectAllPipeline' conn _cart_users db 1000 (\c -> (_cart_usersFirst_name c) `like_` "J%") $
           (CL.map (\record -> F.rcast @["_cart_usersEmail" F.:-> Text, "_cart_usersIs_member" F.:-> Bool] record))
  mapM_ print res
