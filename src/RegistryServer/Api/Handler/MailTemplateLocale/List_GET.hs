module RegistryServer.Api.Handler.MailTemplateLocale.List_GET where

import Servant

import RegistryPublic.Api.Resource.Locale.LocaleDTO
import RegistryPublic.Api.Resource.Locale.LocaleJM ()
import RegistryServer.Api.Handler.Common
import RegistryServer.Model.Context.ContextLenses ()
import RegistryServer.Model.Context.ServerContext
import RegistryServer.Service.MailTemplateLocale.RegistryMailTemplateLocaleService
import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState

list_GET :: Maybe String -> Maybe String -> ServerContextM (Headers '[Header "x-trace-uuid" String] [LocaleDTO])
list_GET mTokenHeader mAppVersion =
  getMaybeAuthServiceExecutor mTokenHeader $ \runInMaybeAuthService ->
    runInMaybeAuthService NoTransaction $ addTraceUuidHeader =<< getRegistryMailTemplateLocales mAppVersion
