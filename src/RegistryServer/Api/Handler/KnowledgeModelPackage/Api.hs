module RegistryServer.Api.Handler.KnowledgeModelPackage.Api where

import Servant
import Servant.Swagger.Tags

import RegistryPublic.Api.Handler.KnowledgeModelPackage.List_Bundle_POST
import RegistryPublic.Api.Handler.KnowledgeModelPackage.List_GET
import RegistryServer.Api.Handler.KnowledgeModelPackage.Detail_Bundle_GET
import RegistryServer.Api.Handler.KnowledgeModelPackage.Detail_GET
import RegistryServer.Api.Handler.KnowledgeModelPackage.List_Bundle_POST
import RegistryServer.Api.Handler.KnowledgeModelPackage.List_GET
import RegistryServer.Model.Context.ServerContext

type KnowledgeModelPackageAPI =
  Tags "Knowledge Model Package"
    :> ( List_GET
           :<|> List_Bundle_POST
           :<|> Detail_GET
           :<|> Detail_Bundle_GET
       )

knowledgeModelPackageApi :: Proxy KnowledgeModelPackageAPI
knowledgeModelPackageApi = Proxy

knowledgeModelPackageServer :: ServerT KnowledgeModelPackageAPI ServerContextM
knowledgeModelPackageServer = list_GET :<|> list_bundle_POST :<|> detail_GET :<|> detail_bundle_GET
