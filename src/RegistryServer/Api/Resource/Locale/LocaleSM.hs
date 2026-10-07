module RegistryServer.Api.Resource.Locale.LocaleSM where

import Data.Swagger

import RegistryPublic.Api.Resource.Locale.LocaleDTO
import RegistryPublic.Api.Resource.Locale.LocaleJM ()
import RegistryServer.Database.Migration.Development.Locale.Data.RegistryLocales
import RegistryServer.Service.Locale.RegistryLocaleMapper
import Shared.Util.Swagger

instance ToSchema LocaleDTO where
  declareNamedSchema = toSwagger (toDTO registryLocaleDe)
