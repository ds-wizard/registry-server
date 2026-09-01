module RegistryServer.Api.Handler.Organization.Detail_DELETE where

import Servant

import RegistryServer.Api.Handler.Common
import RegistryServer.Model.Context.ContextLenses ()
import RegistryServer.Model.Context.ServerContext
import RegistryServer.Service.Organization.OrganizationService
import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState

type Detail_DELETE =
  Header "Authorization" String
    :> "organizations"
    :> Capture "orgId" String
    :> Verb DELETE 204 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] NoContent)

detail_DELETE :: Maybe String -> String -> ServerContextM (Headers '[Header "x-trace-uuid" String] NoContent)
detail_DELETE mTokenHeader orgId =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< do
        deleteOrganization orgId
        return NoContent
