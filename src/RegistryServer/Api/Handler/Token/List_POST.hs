module RegistryServer.Api.Handler.Token.List_POST where

import Servant

import RegistryServer.Api.Handler.Common
import RegistryServer.Api.Resource.UserToken.LoginDTO
import RegistryServer.Api.Resource.UserToken.LoginJM ()
import RegistryServer.Api.Resource.UserToken.UserTokenDTO
import RegistryServer.Api.Resource.UserToken.UserTokenJM ()
import RegistryServer.Model.Context.ContextLenses ()
import RegistryServer.Model.Context.ServerContext
import RegistryServer.Service.UserToken.Login.LoginService
import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState

type List_POST =
  ReqBody '[SafeJSON] LoginDTO
    :> "tokens"
    :> Verb 'POST 201 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] UserTokenDTO)

list_POST :: LoginDTO -> ServerContextM (Headers '[Header "x-trace-uuid" String] UserTokenDTO)
list_POST reqDto =
  runInUnauthService Transactional $ addTraceUuidHeader =<< createLoginTokenFromCredentials reqDto
