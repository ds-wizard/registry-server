module RegistryServer.Api.Handler.Locale.Detail_Po_GET where

import Servant

import RegistryServer.Api.Handler.Common
import RegistryServer.Model.Context.ContextLenses ()
import RegistryServer.Model.Context.ServerContext
import RegistryServer.Service.Locale.RegistryLocaleService
import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState

detail_po_GET :: Maybe String -> String -> String -> ServerContextM (Headers '[Header "x-trace-uuid" String] FileStream)
detail_po_GET mTokenHeader code version =
  getMaybeAuthServiceExecutor mTokenHeader $ \runInMaybeAuthService ->
    runInMaybeAuthService NoTransaction $ addTraceUuidHeader . FileStream =<< getRegistryLocalePo code version
