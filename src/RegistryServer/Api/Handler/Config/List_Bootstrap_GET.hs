module RegistryServer.Api.Handler.Config.List_Bootstrap_GET where

import Servant

import RegistryServer.Api.Handler.Common
import RegistryServer.Api.Resource.Config.ClientConfigDTO
import RegistryServer.Api.Resource.Config.ClientConfigJM ()
import RegistryServer.Model.Context.ServerContext
import RegistryServer.Service.Config.Client.ClientConfigService
import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState

type List_Bootstrap_GET =
  "configs"
    :> "bootstrap"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] ClientConfigDTO)

list_bootstrap_GET :: ServerContextM (Headers '[Header "x-trace-uuid" String] ClientConfigDTO)
list_bootstrap_GET =
  runInUnauthService NoTransaction $ addTraceUuidHeader =<< getClientConfig
