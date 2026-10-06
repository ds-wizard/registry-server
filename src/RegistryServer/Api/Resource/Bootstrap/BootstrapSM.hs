module RegistryServer.Api.Resource.Bootstrap.BootstrapSM where

import Data.Swagger

import RegistryServer.Api.Resource.Bootstrap.BootstrapDTO
import RegistryServer.Api.Resource.Bootstrap.BootstrapJM ()
import qualified RegistryServer.Model.Config.ServerConfigDM as S
import RegistryServer.Service.Bootstrap.BootstrapMapper
import Shared.Util.Swagger

instance ToSchema BootstrapDTO where
  declareNamedSchema = toSwagger (toBootstrapDTO S.defaultConfig)

instance ToSchema BootstrapAuthenticationDTO where
  declareNamedSchema = toSwagger (toBootstrapAuthenticationDTO S.defaultConfig)

instance ToSchema BootstrapLocaleDTO where
  declareNamedSchema = toSwagger (toBootstrapLocaleDTO S.defaultConfig)
