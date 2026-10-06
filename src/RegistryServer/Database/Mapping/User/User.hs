module RegistryServer.Database.Mapping.User.User where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField

import RegistryServer.Model.User.User
import Shared.Database.Mapping.Common

instance ToField UserRole where
  toField = toFieldGenericEnum

instance FromField UserRole where
  fromField = fromFieldGenericEnum

instance ToRow User

instance FromRow User
