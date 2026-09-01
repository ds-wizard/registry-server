module RegistryServer.Database.Mapping.UserEmailLink.UserEmailLinkType where

import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField

import RegistryServer.Model.UserEmailLink.UserEmailLinkType
import Shared.Database.Mapping.Common

instance ToField UserEmailLinkType where
  toField = toFieldGenericEnum

instance FromField UserEmailLinkType where
  fromField = fromFieldGenericEnum
