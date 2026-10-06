module RegistryServer.Api.Handler.User.Detail_Password_PUT where

import qualified Data.UUID as U
import Servant

import RegistryServer.Api.Handler.Common
import RegistryServer.Api.Resource.User.UserPasswordDTO
import RegistryServer.Api.Resource.User.UserPasswordJM ()
import RegistryServer.Model.Context.ContextLenses ()
import RegistryServer.Model.Context.ServerContext
import RegistryServer.Service.User.UserService
import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState

type Detail_Password_PUT =
  Header "Authorization" String
    :> ReqBody '[SafeJSON] UserPasswordDTO
    :> "users"
    :> Capture "uuid" U.UUID
    :> "password"
    :> QueryParam "hash" String
    :> Verb 'PUT 204 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] NoContent)

detail_password_PUT
  :: Maybe String
  -> UserPasswordDTO
  -> U.UUID
  -> Maybe String
  -> ServerContextM (Headers '[Header "x-trace-uuid" String] NoContent)
detail_password_PUT mTokenHeader reqDto uuid mHash =
  getMaybeAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< do
        changeUserPasswordByAdminOrHash uuid reqDto mHash
        return NoContent
