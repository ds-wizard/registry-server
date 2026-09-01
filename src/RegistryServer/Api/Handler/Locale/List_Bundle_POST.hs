module RegistryServer.Api.Handler.Locale.List_Bundle_POST where

import Servant
import Servant.Multipart

import RegistryPublic.Api.Resource.Locale.LocaleDTO
import RegistryPublic.Api.Resource.Locale.LocaleJM ()
import RegistryServer.Api.Handler.Common
import RegistryServer.Model.Context.ServerContext
import RegistryServer.Service.Locale.Bundle.LocaleBundleService
import Shared.Api.Handler.Common
import Shared.Api.Resource.Common.FileDTO
import Shared.Api.Resource.Common.FileJM ()
import Shared.Model.Context.TransactionState

type List_Bundle_POST =
  Header "Authorization" String
    :> MultipartForm Mem FileDTO
    :> "locales"
    :> "bundle"
    :> PostCreated '[SafeJSON] (Headers '[Header "x-trace-uuid" String] LocaleDTO)

list_bundle_POST
  :: Maybe String
  -> FileDTO
  -> ServerContextM (Headers '[Header "x-trace-uuid" String] LocaleDTO)
list_bundle_POST mTokenHeader reqDto =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< do
        importBundle reqDto.content
