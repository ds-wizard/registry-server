module RegistryServer.Database.Mapping.UserToken.UserToken where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField

import RegistryServer.Model.UserToken.UserToken
import Shared.Database.Mapping.Common

instance ToField UserTokenType where
  toField = toFieldGenericEnum

instance FromField UserTokenType where
  fromField = fromFieldGenericEnum

instance ToRow UserToken

instance FromRow UserToken
