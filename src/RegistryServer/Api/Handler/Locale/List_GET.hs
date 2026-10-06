module RegistryServer.Api.Handler.Locale.List_GET where

import Data.Maybe (maybeToList)
import Servant

import RegistryPublic.Api.Resource.Locale.LocaleDTO
import RegistryPublic.Api.Resource.Locale.LocaleJM ()
import RegistryServer.Api.Handler.Common
import RegistryServer.Model.Context.ContextLenses ()
import RegistryServer.Model.Context.ServerContext
import RegistryServer.Service.Locale.LocaleService
import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState

list_GET
  :: Maybe String
  -> Maybe String
  -> Maybe String
  -> ServerContextM (Headers '[Header "x-trace-uuid" String] [LocaleDTO])
list_GET mTokenHeader mId mRecommendedAppVersion =
  getMaybeAuthServiceExecutor mTokenHeader $ \runInMaybeAuthService ->
    runInMaybeAuthService NoTransaction $
      addTraceUuidHeader =<< do
        let queryParams = maybeToList ((,) "id" <$> mId)
        getLocales queryParams mRecommendedAppVersion
