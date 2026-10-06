module RegistryServer.Api.Handler.User.List_Current_Password_PUT where

import Servant

import RegistryServer.Api.Handler.Common hiding (getCurrentUser)
import RegistryServer.Api.Resource.User.UserPasswordDTO
import RegistryServer.Api.Resource.User.UserPasswordJM ()
import RegistryServer.Model.Context.ContextLenses ()
import RegistryServer.Model.Context.RequestContextHelpers
import RegistryServer.Model.Context.ServerContext
import RegistryServer.Model.User.User
import RegistryServer.Service.User.Profile.UserProfileService
import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState

type List_Current_Password_PUT =
  Header "Authorization" String
    :> ReqBody '[SafeJSON] UserPasswordDTO
    :> "users"
    :> "current"
    :> "password"
    :> Verb 'PUT 204 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] NoContent)

list_current_password_PUT :: Maybe String -> UserPasswordDTO -> ServerContextM (Headers '[Header "x-trace-uuid" String] NoContent)
list_current_password_PUT mTokenHeader reqDto =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< do
        user <- getCurrentUser
        changeUserProfilePassword user.uuid reqDto
        return NoContent
