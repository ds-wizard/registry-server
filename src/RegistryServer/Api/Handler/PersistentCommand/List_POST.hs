module RegistryServer.Api.Handler.PersistentCommand.List_POST where

import Servant

import RegistryServer.Api.Handler.Common
import RegistryServer.Model.Context.ServerContext
import RegistryServer.Service.PersistentCommand.PersistentCommandService
import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState
import Shared.Model.PersistentCommand.PersistentCommand

list_POST
  :: Maybe String
  -> PersistentCommand String
  -> ServerContextM (Headers '[Header "x-trace-uuid" String] (PersistentCommand String))
list_POST mTokenHeader reqDto =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< createPersistentCommand reqDto
