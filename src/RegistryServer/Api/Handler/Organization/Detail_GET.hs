module RegistryServer.Api.Handler.Organization.Detail_GET where

import Servant

import RegistryPublic.Api.Resource.Organization.OrganizationDTO
import RegistryPublic.Api.Resource.Organization.OrganizationJM ()
import RegistryServer.Api.Handler.Common
import RegistryServer.Model.Context.ContextLenses ()
import RegistryServer.Model.Context.ServerContext
import RegistryServer.Service.Organization.OrganizationService
import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState

type Detail_GET =
  Header "Authorization" String
    :> "organizations"
    :> Capture "orgId" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] OrganizationDTO)

detail_GET :: Maybe String -> String -> ServerContextM (Headers '[Header "x-trace-uuid" String] OrganizationDTO)
detail_GET mTokenHeader orgId =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getOrganizationByOrgId orgId
