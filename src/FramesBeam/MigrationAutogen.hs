{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module FramesBeam.MigrationAutogen (
  genBeamSchema
) where

import qualified Data.Text           as T
import           Language.Haskell.TH
import           System.Process

go :: String -> Q String
go connString = do
  _ <- runIO $ readCreateProcess (shell ("touch src/Schema.hs")) ""
  code <- runIO $ getCode connString
  return code

genBeamSchema :: String -> Q [Dec]
genBeamSchema str = do
  code <-  go str
  _ <- runIO $ readCreateProcess (shell ("echo " ++ (show code) ++ " > src/Schema.hs")) ""
  [d| id x = x |]



getCode :: String -> IO String
getCode connString = do
  codeString <- readCreateProcess mycmd ""
  return $ sanitizeCode codeString
  where
    mycmd =
      shell
        ("stack exec -- beam-migrate simple schema --backend Database.Beam.Postgres.Migrate --connection " ++ (show connString))

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




