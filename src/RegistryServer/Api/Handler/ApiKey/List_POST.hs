module RegistryServer.Api.Handler.ApiKey.List_POST where

import Servant

import RegistryServer.Api.Handler.Common
import RegistryServer.Api.Resource.UserToken.ApiKeyCreateDTO
import RegistryServer.Api.Resource.UserToken.ApiKeyCreateJM ()
import RegistryServer.Api.Resource.UserToken.UserTokenDTO
import RegistryServer.Api.Resource.UserToken.UserTokenJM ()
import RegistryServer.Model.Context.ContextLenses ()
import RegistryServer.Model.Context.ServerContext
import RegistryServer.Service.UserToken.ApiKey.ApiKeyService
import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState

type List_POST =
  Header "Authorization" String
    :> ReqBody '[SafeJSON] ApiKeyCreateDTO
    :> "api-keys"
    :> Verb 'POST 201 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] UserTokenDTO)

list_POST :: Maybe String -> ApiKeyCreateDTO -> ServerContextM (Headers '[Header "x-trace-uuid" String] UserTokenDTO)
list_POST mTokenHeader reqDto =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< createApiKey reqDto
