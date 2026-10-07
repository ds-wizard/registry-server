module RegistryServer.Database.Mapping.Locale.RegistryLocale where

import Database.PostgreSQL.Simple

import RegistryServer.Model.Locale.RegistryLocale

instance ToRow RegistryLocale

instance FromRow RegistryLocale
