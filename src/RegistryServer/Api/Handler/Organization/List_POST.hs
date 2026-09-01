module RegistryServer.Api.Handler.Organization.List_POST where

import Servant

import RegistryPublic.Api.Resource.Organization.OrganizationCreateDTO
import RegistryPublic.Api.Resource.Organization.OrganizationCreateJM ()
import RegistryPublic.Api.Resource.Organization.OrganizationDTO
import RegistryPublic.Api.Resource.Organization.OrganizationJM ()
import RegistryServer.Api.Handler.Common
import RegistryServer.Model.Context.ContextLenses ()
import RegistryServer.Model.Context.ServerContext
import RegistryServer.Service.Organization.OrganizationService
import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState

list_POST :: Maybe String -> OrganizationCreateDTO -> Maybe String -> ServerContextM (Headers '[Header "x-trace-uuid" String] OrganizationDTO)
list_POST mTokenHeader reqDto mCallbackUrl =
  getMaybeAuthServiceExecutor mTokenHeader $ \runInMaybeAuthService ->
    runInMaybeAuthService Transactional $
      addTraceUuidHeader =<< createOrganization reqDto mCallbackUrl
