module RegistryServer.Api.Handler.User.Detail_State_PUT where

import qualified Data.UUID as U
import Servant

import RegistryServer.Api.Handler.Common
import RegistryServer.Api.Resource.User.UserStateDTO
import RegistryServer.Api.Resource.User.UserStateJM ()
import RegistryServer.Model.Context.ContextLenses ()
import RegistryServer.Model.Context.ServerContext
import RegistryServer.Service.User.UserService
import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState

type Detail_State_PUT =
  Header "Authorization" String
    :> ReqBody '[SafeJSON] UserStateDTO
    :> "users"
    :> Capture "uuid" U.UUID
    :> "state"
    :> QueryParam' '[Required] "hash" String
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] UserStateDTO)

detail_state_PUT
  :: Maybe String
  -> UserStateDTO
  -> U.UUID
  -> String
  -> ServerContextM (Headers '[Header "x-trace-uuid" String] UserStateDTO)
detail_state_PUT mTokenHeader reqDto _ hash =
  getMaybeAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< do
        changeUserState hash reqDto.active
        return reqDto
