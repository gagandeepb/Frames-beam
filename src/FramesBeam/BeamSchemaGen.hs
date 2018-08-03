{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module FramesBeam.BeamSchemaGen (
  genBeamSchema,
  genBeamSchemaForTests,
  getCode
) where

import qualified Data.ByteString                as B
import qualified Data.Text                      as T
import           Database.Beam.Migrate.Simple   (haskellSchema)
import           Database.Beam.Postgres         (runBeamPostgres)
import           Database.Beam.Postgres.Migrate (migrationBackend)
import qualified Database.PostgreSQL.Simple     as Pg
import           Language.Haskell.TH
import           System.Process

data FolderName = Src | Test

instance Show FolderName where
  show Src  = "src"
  show Test = "test"

go :: B.ByteString -> FolderName -> Q String
go connString folder = do
  _ <- runIO $ readCreateProcess (shell ("touch " ++ (show folder) ++ "/NewBeamSchema.hs")) ""
  code <- runIO $ getCode connString
  return code

genBeamSchemaHelper :: B.ByteString -> FolderName -> Q [Dec]
genBeamSchemaHelper str folder = do
  code <-  go str folder
  _ <- runIO $ readCreateProcess (shell ("echo " ++ (show code) ++ " > " ++ (show folder) ++ "/NewBeamSchema.hs")) ""
  [d| id x = x |]

genBeamSchema :: B.ByteString -> Q [Dec]
genBeamSchema str = genBeamSchemaHelper str Src

genBeamSchemaForTests :: B.ByteString -> Q [Dec]
genBeamSchemaForTests str = genBeamSchemaHelper str Test

getCode :: B.ByteString -> IO String
getCode connString = do
  conn <- Pg.connectPostgreSQL connString
  codeString <- runBeamPostgres conn (haskellSchema migrationBackend)
  return $ sanitizeCode codeString

sanitizeCode :: String -> String
sanitizeCode str =
  addExts ++ (T.unpack moduleNameAndImports) ++ addImports ++ (T.unpack beforeMigrationTySig) ++
    "\nmigration :: Migration PgCommandSyntax (CheckedDatabaseSettings Postgres Db)\n" ++
    (T.unpack includeThisChunk) ++
    "\ndb :: DatabaseSettings Postgres Db\ndb = unCheckDatabase (runMigrationSilenced (migration))\n"
  where
    txt = T.pack str
    (_exts, remainingCode) = T.breakOn "module" txt
    (moduleNameAndImports, rest) = T.breakOn "data" remainingCode
    (beforeMigrationTySig, afterIncludingMigrationTySig) = T.breakOn "migration ::" rest
    (_migrationTySig, afterIncludingMigrationDecl) = T.breakOn "migration\n  =" afterIncludingMigrationTySig
    (includeThisChunk, _discardThisChunk) = T.breakOn "db ::" afterIncludingMigrationDecl

addExts :: String
addExts =
  "{-# LANGUAGE StandaloneDeriving    #-}\n" ++
    "{-# LANGUAGE DeriveGeneric         #-}\n" ++
    "{-# LANGUAGE ExplicitNamespaces    #-}\n" ++
    "{-# LANGUAGE FlexibleContexts      #-}\n" ++
    "{-# LANGUAGE FlexibleInstances     #-}\n" ++
    "{-# LANGUAGE GADTs                 #-}\n" ++
    "{-# LANGUAGE MultiParamTypeClasses #-}\n" ++
    "{-# LANGUAGE OverloadedStrings     #-}\n" ++
    "{-# LANGUAGE ScopedTypeVariables   #-}\n" ++
    "{-# LANGUAGE StandaloneDeriving    #-}\n" ++
    "{-# LANGUAGE TypeFamilies          #-}\n" ++
    "{-# LANGUAGE TypeSynonymInstances  #-}\n"


addImports :: String
addImports =
  "import           Database.Beam.Postgres\n\n\n"




