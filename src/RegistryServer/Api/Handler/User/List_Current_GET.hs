module RegistryServer.Api.Handler.User.List_Current_GET where

import Servant

import RegistryServer.Api.Handler.Common
import RegistryServer.Api.Resource.User.UserDTO
import RegistryServer.Api.Resource.User.UserJM ()
import RegistryServer.Model.Context.ContextLenses ()
import RegistryServer.Model.Context.ServerContext
import RegistryServer.Service.User.Profile.UserProfileService
import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState

type List_Current_GET =
  Header "Authorization" String
    :> "users"
    :> "current"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] UserDTO)

list_current_GET :: Maybe String -> ServerContextM (Headers '[Header "x-trace-uuid" String] UserDTO)
list_current_GET mTokenHeader =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getUserProfile
