module RegistryServer.Api.Handler.User.Detail_DELETE where

import qualified Data.UUID as U
import Servant

import RegistryServer.Api.Handler.Common
import RegistryServer.Model.Context.ContextLenses ()
import RegistryServer.Model.Context.ServerContext
import RegistryServer.Service.User.UserService
import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState

type Detail_DELETE =
  Header "Authorization" String
    :> "users"
    :> Capture "uuid" U.UUID
    :> Verb DELETE 204 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] NoContent)

detail_DELETE :: Maybe String -> U.UUID -> ServerContextM (Headers '[Header "x-trace-uuid" String] NoContent)
detail_DELETE mTokenHeader uuid =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< do
        deleteUser uuid
        return NoContent
