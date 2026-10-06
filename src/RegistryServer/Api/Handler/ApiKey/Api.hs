module RegistryServer.Api.Handler.ApiKey.Api where

import Servant
import Servant.Swagger.Tags

import RegistryServer.Api.Handler.ApiKey.Detail_DELETE
import RegistryServer.Api.Handler.ApiKey.List_GET
import RegistryServer.Api.Handler.ApiKey.List_POST
import RegistryServer.Model.Context.ServerContext

type ApiKeyAPI =
  Tags "ApiKey"
    :> ( List_GET
           :<|> List_POST
           :<|> Detail_DELETE
       )

apiKeyApi :: Proxy ApiKeyAPI
apiKeyApi = Proxy

apiKeyServer :: ServerT ApiKeyAPI ServerContextM
apiKeyServer =
  list_GET
    :<|> list_POST
    :<|> detail_DELETE
