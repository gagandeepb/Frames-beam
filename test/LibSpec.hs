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
module LibSpec where

import           Test.Hspec
import           Test.Hspec.Core.Util

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
import           GHC.Exception                  (SomeException)

import           NewBeamSchema


$(genBeamSchemaForTests "host=localhost dbname=shoppingcart1")

deriveGeneric ''Cart_usersT

deriveVinyl ''Cart_usersT

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Integration Tests (Requires DB environment)" $ do
    it "test0" $ do
      res <- safeTry $ test0
      (formatEx res) `shouldBe` (NoExceptionRaised)
    it "testStream1" $ do
      res <- safeTry $ testStream1 2
      (formatEx res) `shouldBe` (NoExceptionRaised)
    it "testStream2" $ do
      res <- safeTry $ testStream2 2
      (formatEx res) `shouldBe` (NoExceptionRaised)
    it "testStream3" $ do
      res <- safeTry $ testStream3 2
      (formatEx res) `shouldBe` (NoExceptionRaised)
    it "testStream4" $ do
      res <- safeTry $ testStream4
      (formatEx res) `shouldBe` (NoExceptionRaised)
    it "testStream5" $ do
      res <- safeTry $ testStream5
      (formatEx res) `shouldBe` (NoExceptionRaised)
    it "testStream6" $ do
      res <- safeTry $ testStream6
      (formatEx res) `shouldBe` (NoExceptionRaised)



data ExceptionStatus = ExceptionRaised String | NoExceptionRaised deriving (Show, Eq)

formatEx :: Either SomeException b -> ExceptionStatus
formatEx (Left e)  = ExceptionRaised (formatException e)
formatEx (Right _) = NoExceptionRaised

test0 :: IO ()
test0 = do
  us <- testStream1 5
  mapM_ print $ F.toFrame $ map createRecId us

testStream1 :: Int -> IO [(Cart_usersT Identity)]
testStream1 n = do
  conn <- connectPostgreSQL  "host=localhost dbname=shoppingcart1"
  res <- DBPC.runSelect conn (select (allRows _cart_users db)) (\c -> runConduit $ c .| CL.take n)
  return res

testStream2 :: Int -> IO ()
testStream2 n = do
  conn <- connectPostgreSQL  "host=localhost dbname=shoppingcart1"
  res <- DBPC.runSelect conn (select (allRowsWhere _cart_users db (\c -> (_cart_usersFirst_name c) `like_` "J%") )) (\c -> runConduit $ c .| CL.take n)
  mapM_ print $ map createRecId  res


testStream3 :: Int -> IO [(Cart_usersT Identity)]
testStream3 n = do
  conn <- connectPostgreSQL  "host=localhost dbname=shoppingcart1"
  res <- DBPC.runSelect conn (select (allRowsWhere _cart_users db (\c -> (_cart_usersFirst_name c) `like_` "J%") )) (\c -> runConduit $ c .| CL.take n)
  return res


testStream4 :: IO ()
testStream4 = do
  conn <- connectPostgreSQL  "host=localhost dbname=shoppingcart1"
  res <- bulkSelectAllRows conn _cart_users db 1000
  mapM_ (print . createRecId) res


testStream5 :: IO ()
testStream5 = do
  conn <- connectPostgreSQL  "host=localhost dbname=shoppingcart1"
  res <- streamingSelectAllPipeline' conn _cart_users db 1000 (\c -> (_cart_usersFirst_name c) `like_` "J%") (CL.isolate 1000)
  mapM_ print res

-- Streaming column-subset
testStream6 :: IO ()
testStream6 = do
  conn <- connectPostgreSQL  "host=localhost dbname=shoppingcart1"
  res <- streamingSelectAllPipeline' conn _cart_users db 1000 (\c -> (_cart_usersFirst_name c) `like_` "J%") $
           (CL.map (\record -> F.rcast @["_cart_usersEmail" F.:-> Text, "_cart_usersIs_member" F.:-> Bool] record))
  mapM_ print res


