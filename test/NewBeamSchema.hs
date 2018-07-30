{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE ExplicitNamespaces    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module NewBeamSchema
       (db, migration, Db(..), Cart_usersT(..), Cart_usersT,
        Cart_usersKey)
       where
import Database.Beam
import Control.Applicative
import Data.Text (Text)
import Database.Beam.Migrate
       (CheckedDatabaseSettings, Migration, Sql92SaneDdlCommandSyntax,
        boolean, createTable, field, int, notNull, runMigrationSilenced,
        unCheckDatabase, varchar)

import           Database.Beam.Postgres


data Cart_usersT f = Cart_users{_cart_usersEmail ::
                                Columnar f Text,
                                _cart_usersFirst_name :: Columnar f Text,
                                _cart_usersLast_name :: Columnar f Text,
                                _cart_usersIs_member :: Columnar f Bool,
                                _cart_usersDays_in_queue :: Columnar f Int}
                       deriving Generic

instance Beamable Cart_usersT

type Cart_users = Cart_usersT Identity

deriving instance Show Cart_users

deriving instance Eq Cart_users

instance Table Cart_usersT where
        data PrimaryKey Cart_usersT f = Cart_usersKey (Columnar f Text)
                                          deriving Generic
        primaryKey = Cart_usersKey <$> _cart_usersEmail

type Cart_usersKey = PrimaryKey Cart_usersT Identity

instance Beamable (PrimaryKey Cart_usersT)

deriving instance Eq Cart_usersKey

deriving instance Show Cart_usersKey

data Db entity = Db{_cart_users ::
                    entity (TableEntity Cart_usersT)}
                   deriving Generic


migration :: Migration PgCommandSyntax (CheckedDatabaseSettings Postgres Db)
migration
  = do cart_users <- createTable "cart_users"
                       (Cart_users (field "email" (varchar Nothing) notNull)
                          (field "first_name" (varchar Nothing) notNull)
                          (field "last_name" (varchar Nothing) notNull)
                          (field "is_member" boolean notNull)
                          (field "days_in_queue" int notNull))
       pure Db{_cart_users = cart_users}

instance Database be Db


db :: DatabaseSettings Postgres Db
db = unCheckDatabase (runMigrationSilenced (migration))

