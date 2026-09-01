module RegistryServer.Api.Handler.Organization.List_Simple_GET where

import Servant

import RegistryPublic.Api.Resource.Organization.OrganizationSimpleJM ()
import RegistryPublic.Model.Organization.OrganizationSimple
import RegistryServer.Api.Handler.Common
import RegistryServer.Model.Context.ContextLenses ()
import RegistryServer.Model.Context.ServerContext
import RegistryServer.Service.Organization.OrganizationService
import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState

list_simple_GET :: ServerContextM (Headers '[Header "x-trace-uuid" String] [OrganizationSimple])
list_simple_GET = runInUnauthService NoTransaction $ addTraceUuidHeader =<< getSimpleOrganizations
