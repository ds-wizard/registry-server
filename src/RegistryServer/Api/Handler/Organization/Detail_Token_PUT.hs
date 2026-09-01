module RegistryServer.Api.Handler.Organization.Detail_Token_PUT where

import Servant

import RegistryPublic.Api.Resource.Organization.OrganizationDTO
import RegistryPublic.Api.Resource.Organization.OrganizationJM ()
import RegistryServer.Api.Handler.Common
import RegistryServer.Model.Context.ContextLenses ()
import RegistryServer.Model.Context.ServerContext
import RegistryServer.Service.Organization.OrganizationService
import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState

type Detail_Token_PUT =
  "organizations"
    :> Capture "orgId" String
    :> "token"
    :> QueryParam' '[Required] "hash" String
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] OrganizationDTO)

detail_token_PUT :: String -> String -> ServerContextM (Headers '[Header "x-trace-uuid" String] OrganizationDTO)
detail_token_PUT orgId hash =
  runInUnauthService Transactional $ addTraceUuidHeader =<< changeOrganizationTokenByHash orgId hash
