module RegistryServer.Api.Handler.User.Detail_GET where

import qualified Data.UUID as U
import Servant

import RegistryServer.Api.Handler.Common
import RegistryServer.Api.Resource.User.UserDTO
import RegistryServer.Api.Resource.User.UserJM ()
import RegistryServer.Model.Context.ContextLenses ()
import RegistryServer.Model.Context.ServerContext
import RegistryServer.Service.User.UserService
import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState

type Detail_GET =
  Header "Authorization" String
    :> "users"
    :> Capture "uuid" U.UUID
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] UserDTO)

detail_GET :: Maybe String -> U.UUID -> ServerContextM (Headers '[Header "x-trace-uuid" String] UserDTO)
detail_GET mTokenHeader uuid =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getUserDetailById uuid
