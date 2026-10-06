module RegistryServer.Api.Handler.Bootstrap.List_GET where

import Servant

import RegistryServer.Api.Handler.Common
import RegistryServer.Api.Resource.Bootstrap.BootstrapDTO
import RegistryServer.Api.Resource.Bootstrap.BootstrapJM ()
import RegistryServer.Model.Context.ServerContext
import RegistryServer.Service.Bootstrap.BootstrapService
import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState

type List_GET =
  "bootstrap"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] BootstrapDTO)

list_GET :: ServerContextM (Headers '[Header "x-trace-uuid" String] BootstrapDTO)
list_GET =
  runInUnauthService NoTransaction $ addTraceUuidHeader =<< getBootstrap
