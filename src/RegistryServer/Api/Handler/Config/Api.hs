module RegistryServer.Api.Handler.Config.Api where

import Servant

import RegistryServer.Api.Handler.Config.List_Bootstrap_GET
import RegistryServer.Model.Context.ServerContext

type ConfigAPI =
  List_Bootstrap_GET

configApi :: Proxy ConfigAPI
configApi = Proxy

configServer :: ServerT ConfigAPI ServerContextM
configServer = list_bootstrap_GET
