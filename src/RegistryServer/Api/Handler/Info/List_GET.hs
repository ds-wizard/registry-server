module RegistryServer.Api.Handler.Info.List_GET where

import Servant

import RegistryServer.Api.Handler.Common
import RegistryServer.Model.Context.ContextLenses ()
import RegistryServer.Model.Context.ServerContext hiding (buildInfoConfig)
import Shared.Api.Handler.Common
import Shared.Api.Resource.Info.InfoDTO
import Shared.Api.Resource.Info.InfoJM ()
import Shared.Model.Context.TransactionState
import Shared.Service.Info.InfoService

type List_GET = Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] InfoDTO)

list_GET :: ServerContextM (Headers '[Header "x-trace-uuid" String] InfoDTO)
list_GET =
  runInUnauthService NoTransaction $
    addTraceUuidHeader =<< getInfo []
