module RegistryServer.Api.Handler.DocumentTemplate.Api where

import Servant

import RegistryPublic.Api.Handler.DocumentTemplate.List_GET
import RegistryServer.Api.Handler.DocumentTemplate.Detail_Bundle_GET
import RegistryServer.Api.Handler.DocumentTemplate.Detail_GET
import RegistryServer.Api.Handler.DocumentTemplate.List_Bundle_POST
import RegistryServer.Api.Handler.DocumentTemplate.List_GET
import RegistryServer.Model.Context.ServerContext

type DocumentTemplateAPI =
  List_GET
    :<|> Templates__List_GET
    :<|> List_Bundle_POST
    :<|> Detail_GET
    :<|> Templates__Detail_Bundle_GET
    :<|> Detail_Bundle_GET

documentTemplateApi :: Proxy DocumentTemplateAPI
documentTemplateApi = Proxy

documentTemplateServer :: ServerT DocumentTemplateAPI ServerContextM
documentTemplateServer = list_GET :<|> list_GET :<|> list_bundle_POST :<|> detail_GET :<|> detail_bundle_GET :<|> detail_bundle_GET
