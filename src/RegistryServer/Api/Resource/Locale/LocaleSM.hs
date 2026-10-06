module RegistryServer.Api.Resource.Locale.LocaleSM where

import Data.Swagger

import RegistryPublic.Api.Resource.Locale.LocaleDTO
import RegistryPublic.Api.Resource.Locale.LocaleJM ()
import RegistryServer.Service.Locale.LocaleMapper
import Shared.Database.Migration.Development.Locale.Data.Locales
import Shared.Util.Swagger

instance ToSchema LocaleDTO where
  declareNamedSchema = toSwagger (toDTO localeNl)
