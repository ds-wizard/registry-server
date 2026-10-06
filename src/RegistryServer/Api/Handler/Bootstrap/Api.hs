module RegistryServer.Api.Handler.Bootstrap.Api where

import Servant
import Servant.Swagger.Tags

import RegistryServer.Api.Handler.Bootstrap.List_GET
import RegistryServer.Model.Context.ServerContext

type BootstrapAPI =
  Tags "Bootstrap"
    :> List_GET

bootstrapApi :: Proxy BootstrapAPI
bootstrapApi = Proxy

bootstrapServer :: ServerT BootstrapAPI ServerContextM
bootstrapServer = list_GET
