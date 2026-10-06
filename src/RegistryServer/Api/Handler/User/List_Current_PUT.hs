module RegistryServer.Api.Handler.User.List_Current_PUT where

import Servant

import RegistryServer.Api.Handler.Common
import RegistryServer.Api.Resource.User.UserDTO
import RegistryServer.Api.Resource.User.UserJM ()
import RegistryServer.Api.Resource.User.UserProfileChangeDTO
import RegistryServer.Api.Resource.User.UserProfileChangeJM ()
import RegistryServer.Model.Context.ContextLenses ()
import RegistryServer.Model.Context.ServerContext
import RegistryServer.Service.User.Profile.UserProfileService
import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState

type List_Current_PUT =
  Header "Authorization" String
    :> ReqBody '[SafeJSON] UserProfileChangeDTO
    :> "users"
    :> "current"
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] UserDTO)

list_current_PUT :: Maybe String -> UserProfileChangeDTO -> ServerContextM (Headers '[Header "x-trace-uuid" String] UserDTO)
list_current_PUT mTokenHeader reqDto =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< modifyUserProfile reqDto
