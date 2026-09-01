module RegistryServer.Api.Handler.Organization.Detail_PUT where

import Servant

import RegistryPublic.Api.Resource.Organization.OrganizationDTO
import RegistryPublic.Api.Resource.Organization.OrganizationJM ()
import RegistryServer.Api.Handler.Common
import RegistryServer.Api.Resource.Organization.OrganizationChangeDTO
import RegistryServer.Api.Resource.Organization.OrganizationChangeJM ()
import RegistryServer.Model.Context.ContextLenses ()
import RegistryServer.Model.Context.ServerContext
import RegistryServer.Service.Organization.OrganizationService
import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState

type Detail_PUT =
  Header "Authorization" String
    :> ReqBody '[SafeJSON] OrganizationChangeDTO
    :> "organizations"
    :> Capture "orgId" String
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] OrganizationDTO)

detail_PUT
  :: Maybe String
  -> OrganizationChangeDTO
  -> String
  -> ServerContextM (Headers '[Header "x-trace-uuid" String] OrganizationDTO)
detail_PUT mTokenHeader reqDto orgId =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< modifyOrganization orgId reqDto
