module RegistryServer.Api.Handler.ApiKey.List_GET where

import Servant

import RegistryServer.Api.Handler.Common
import RegistryServer.Api.Resource.UserToken.UserTokenListJM ()
import RegistryServer.Model.Context.ContextLenses ()
import RegistryServer.Model.Context.ServerContext
import RegistryServer.Model.UserToken.UserToken
import RegistryServer.Model.UserToken.UserTokenList
import RegistryServer.Service.UserToken.UserTokenService
import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState

type List_GET =
  Header "Authorization" String
    :> "api-keys"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] [UserTokenList])

list_GET :: Maybe String -> ServerContextM (Headers '[Header "x-trace-uuid" String] [UserTokenList])
list_GET mTokenHeader =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getTokens ApiKeyUserTokenType
