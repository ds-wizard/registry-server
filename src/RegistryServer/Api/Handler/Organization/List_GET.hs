module RegistryServer.Api.Handler.Organization.List_GET where

import Servant

import RegistryPublic.Api.Resource.Organization.OrganizationDTO
import RegistryPublic.Api.Resource.Organization.OrganizationJM ()
import RegistryServer.Api.Handler.Common
import RegistryServer.Model.Context.ContextLenses ()
import RegistryServer.Model.Context.ServerContext
import RegistryServer.Service.Organization.OrganizationService
import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState

type List_GET =
  Header "Authorization" String
    :> "organizations"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] [OrganizationDTO])

list_GET :: Maybe String -> ServerContextM (Headers '[Header "x-trace-uuid" String] [OrganizationDTO])
list_GET mTokenHeader =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getOrganizations
