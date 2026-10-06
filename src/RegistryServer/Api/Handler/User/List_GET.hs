module RegistryServer.Api.Handler.User.List_GET where

import Servant

import RegistryServer.Api.Handler.Common
import RegistryServer.Api.Resource.User.UserDTO
import RegistryServer.Api.Resource.User.UserJM ()
import RegistryServer.Model.Context.ContextLenses ()
import RegistryServer.Model.Context.ServerContext
import RegistryServer.Service.User.UserService
import Shared.Api.Handler.Common
import Shared.Api.Resource.Common.PageJM ()
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Context.TransactionState

type List_GET =
  Header "Authorization" String
    :> "users"
    :> QueryParam "q" String
    :> QueryParam "role" String
    :> QueryParam "page" Int
    :> QueryParam "size" Int
    :> QueryParam "sort" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] (Page UserDTO))

list_GET
  :: Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe Int
  -> Maybe Int
  -> Maybe String
  -> ServerContextM (Headers '[Header "x-trace-uuid" String] (Page UserDTO))
list_GET mTokenHeader mQuery mRole mPage mSize mSort =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader =<< getUsersPage mQuery mRole (Pageable mPage mSize) (parseSortQuery mSort)
