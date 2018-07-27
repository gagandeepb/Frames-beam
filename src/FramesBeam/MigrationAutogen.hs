{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module FramesBeam.MigrationAutogen (
  genBeamSchema
) where

import qualified Data.ByteString                as B
import qualified Data.Text                      as T
import           Database.Beam.Migrate.Simple   (haskellSchema)
import           Database.Beam.Postgres         (runBeamPostgres)
import           Database.Beam.Postgres.Migrate (migrationBackend)
import qualified Database.PostgreSQL.Simple     as Pg
import           Language.Haskell.TH
import           System.Process

go :: B.ByteString -> Q String
go connString = do
  _ <- runIO $ readCreateProcess (shell ("touch src/NewBeamSchema.hs")) ""
  code <- runIO $ getCode connString
  return code

genBeamSchema :: B.ByteString -> Q [Dec]
genBeamSchema str = do
  code <-  go str
  _ <- runIO $ readCreateProcess (shell ("echo " ++ (show code) ++ " > src/NewBeamSchema.hs")) ""
  [d| id x = x |]

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




