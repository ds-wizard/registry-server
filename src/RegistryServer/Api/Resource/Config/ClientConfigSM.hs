module RegistryServer.Api.Resource.Config.ClientConfigSM where

import Data.Swagger

import RegistryServer.Api.Resource.Config.ClientConfigDTO
import RegistryServer.Api.Resource.Config.ClientConfigJM ()
import qualified RegistryServer.Model.Config.ServerConfigDM as S
import RegistryServer.Service.Config.Client.ClientConfigMapper
import Shared.Util.Swagger

instance ToSchema ClientConfigDTO where
  declareNamedSchema = toSwagger (toClientConfigDTO S.defaultConfig)

instance ToSchema ClientConfigAuthDTO where
  declareNamedSchema = toSwagger (toClientAuthDTO S.defaultConfig)

instance ToSchema ClientConfigLocaleDTO where
  declareNamedSchema = toSwagger (toClientLocaleDTO S.defaultConfig)
