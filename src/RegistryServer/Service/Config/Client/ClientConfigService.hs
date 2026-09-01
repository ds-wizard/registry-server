module RegistryServer.Service.Config.Client.ClientConfigService where

import Control.Monad.Reader (asks)

import RegistryServer.Api.Resource.Config.ClientConfigDTO
import RegistryServer.Model.Context.RequestContext
import RegistryServer.Service.Config.Client.ClientConfigMapper

getClientConfig :: RequestContextM ClientConfigDTO
getClientConfig = do
  serverConfig <- asks serverConfig
  return $ toClientConfigDTO serverConfig
