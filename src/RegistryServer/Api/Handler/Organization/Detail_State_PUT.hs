module RegistryServer.Api.Handler.Organization.Detail_State_PUT where

import Servant

import RegistryPublic.Api.Resource.Organization.OrganizationDTO
import RegistryPublic.Api.Resource.Organization.OrganizationJM ()
import RegistryPublic.Api.Resource.Organization.OrganizationStateDTO
import RegistryPublic.Api.Resource.Organization.OrganizationStateJM ()
import RegistryServer.Api.Handler.Common
import RegistryServer.Model.Context.ContextLenses ()
import RegistryServer.Model.Context.ServerContext
import RegistryServer.Service.Organization.OrganizationService
import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState

detail_state_PUT
  :: OrganizationStateDTO
  -> String
  -> String
  -> ServerContextM (Headers '[Header "x-trace-uuid" String] OrganizationDTO)
detail_state_PUT reqDto orgId hash =
  runInUnauthService Transactional $ addTraceUuidHeader =<< changeOrganizationState orgId hash reqDto
