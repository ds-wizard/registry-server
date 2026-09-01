module RegistryServer.Api.Handler.UserEmailLink.List_POST where

import Servant

import RegistryServer.Api.Handler.Common
import RegistryServer.Api.Resource.UserEmailLink.UserEmailLinkJM ()
import RegistryServer.Model.Context.ContextLenses ()
import RegistryServer.Model.Context.ServerContext
import RegistryServer.Model.UserEmailLink.UserEmailLinkType
import RegistryServer.Service.Organization.OrganizationService
import Shared.Api.Handler.Common
import Shared.Api.Resource.UserEmailLink.UserEmailLinkDTO
import Shared.Api.Resource.UserEmailLink.UserEmailLinkJM ()
import Shared.Model.Context.TransactionState

type List_POST =
  ReqBody '[SafeJSON] (UserEmailLinkDTO UserEmailLinkType)
    :> "user-email-links"
    :> Verb 'POST 201 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] NoContent)

list_POST :: UserEmailLinkDTO UserEmailLinkType -> ServerContextM (Headers '[Header "x-trace-uuid" String] NoContent)
list_POST reqDto =
  runInUnauthService Transactional $
    addTraceUuidHeader =<< do
      resetOrganizationToken reqDto
      return NoContent
