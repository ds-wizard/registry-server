module RegistryServer.Api.Handler.DocumentTemplate.Api where

import Servant
import Servant.Swagger.Tags

import RegistryPublic.Api.Handler.DocumentTemplate.List_GET
import RegistryServer.Api.Handler.DocumentTemplate.Detail_Bundle_GET
import RegistryServer.Api.Handler.DocumentTemplate.Detail_GET
import RegistryServer.Api.Handler.DocumentTemplate.List_Bundle_POST
import RegistryServer.Api.Handler.DocumentTemplate.List_GET
import RegistryServer.Model.Context.ServerContext

type DocumentTemplateAPI =
  Tags "Document Template"
    :> ( List_GET
           :<|> List_Bundle_POST
           :<|> Detail_GET
           :<|> Detail_Bundle_GET
       )

documentTemplateApi :: Proxy DocumentTemplateAPI
documentTemplateApi = Proxy

documentTemplateServer :: ServerT DocumentTemplateAPI ServerContextM
documentTemplateServer = list_GET :<|> list_bundle_POST :<|> detail_GET :<|> detail_bundle_GET
