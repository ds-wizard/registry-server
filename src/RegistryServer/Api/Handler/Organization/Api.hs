module RegistryServer.Api.Handler.Organization.Api where

import Servant

import RegistryPublic.Api.Handler.Organization.Detail_State_PUT
import RegistryPublic.Api.Handler.Organization.List_POST
import RegistryPublic.Api.Handler.Organization.List_Simple_GET
import RegistryServer.Api.Handler.Organization.Detail_DELETE
import RegistryServer.Api.Handler.Organization.Detail_GET
import RegistryServer.Api.Handler.Organization.Detail_PUT
import RegistryServer.Api.Handler.Organization.Detail_State_PUT
import RegistryServer.Api.Handler.Organization.Detail_Token_PUT
import RegistryServer.Api.Handler.Organization.List_GET
import RegistryServer.Api.Handler.Organization.List_POST
import RegistryServer.Api.Handler.Organization.List_Simple_GET
import RegistryServer.Model.Context.ServerContext

type OrganizationAPI =
  List_GET
    :<|> List_Simple_GET
    :<|> List_POST
    :<|> Detail_GET
    :<|> Detail_PUT
    :<|> Detail_DELETE
    :<|> Detail_State_PUT
    :<|> Detail_Token_PUT

organizationApi :: Proxy OrganizationAPI
organizationApi = Proxy

organizationServer :: ServerT OrganizationAPI ServerContextM
organizationServer =
  list_GET
    :<|> list_simple_GET
    :<|> list_POST
    :<|> detail_GET
    :<|> detail_PUT
    :<|> detail_DELETE
    :<|> detail_state_PUT
    :<|> detail_token_PUT
