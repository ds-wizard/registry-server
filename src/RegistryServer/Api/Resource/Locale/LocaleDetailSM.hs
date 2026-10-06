module RegistryServer.Api.Resource.Locale.LocaleDetailSM where

import Data.Swagger

import RegistryServer.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleSM ()
import RegistryServer.Api.Resource.Locale.LocaleDetailDTO
import RegistryServer.Api.Resource.Locale.LocaleDetailJM ()
import RegistryServer.Service.Locale.LocaleMapper
import Shared.Database.Migration.Development.Locale.Data.Locales
import Shared.Util.Swagger

instance ToSchema LocaleDetailDTO where
  declareNamedSchema = toSwagger (toDetailDTO localeNl [])
