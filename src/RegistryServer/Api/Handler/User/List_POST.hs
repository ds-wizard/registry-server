module RegistryServer.Api.Handler.User.List_POST where

import Servant

import RegistryServer.Api.Handler.Common
import RegistryServer.Api.Resource.User.UserCreateDTO
import RegistryServer.Api.Resource.User.UserCreateJM ()
import RegistryServer.Api.Resource.User.UserDTO
import RegistryServer.Api.Resource.User.UserJM ()
import RegistryServer.Model.Context.ContextLenses ()
import RegistryServer.Model.Context.ServerContext
import RegistryServer.Service.User.UserService
import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState

type List_POST =
  Header "Authorization" String
    :> ReqBody '[SafeJSON] UserCreateDTO
    :> "users"
    :> Verb 'POST 201 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] UserDTO)

list_POST :: Maybe String -> UserCreateDTO -> ServerContextM (Headers '[Header "x-trace-uuid" String] UserDTO)
list_POST mTokenHeader reqDto =
  getMaybeAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< registerOrCreateUserByAdmin reqDto
