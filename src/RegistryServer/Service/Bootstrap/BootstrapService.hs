module RegistryServer.Service.Bootstrap.BootstrapService where

import Control.Monad.Reader (asks)

import RegistryServer.Api.Resource.Bootstrap.BootstrapDTO
import RegistryServer.Model.Context.RequestContext
import RegistryServer.Service.Bootstrap.BootstrapMapper

getBootstrap :: RequestContextM BootstrapDTO
getBootstrap = do
  serverConfig <- asks (.serverConfig)
  return $ toBootstrapDTO serverConfig
