module RegistryServer.Api.Handler.DocumentTemplate.List_Bundle_POST where

import Servant
import Servant.Multipart

import RegistryServer.Api.Handler.Common
import RegistryServer.Model.Context.ServerContext
import RegistryServer.Service.DocumentTemplate.Bundle.DocumentTemplateBundleService
import Shared.Api.Handler.Common
import Shared.Api.Resource.Common.FileDTO
import Shared.Api.Resource.Common.FileJM ()
import Shared.Api.Resource.DocumentTemplate.DocumentTemplateSimpleJM ()
import Shared.Model.Context.TransactionState
import Shared.Model.DocumentTemplate.DocumentTemplateSimple

type List_Bundle_POST =
  Header "Authorization" String
    :> MultipartForm Mem FileDTO
    :> "document-templates"
    :> "bundle"
    :> PostCreated '[SafeJSON] (Headers '[Header "x-trace-uuid" String] DocumentTemplateSimple)

list_bundle_POST
  :: Maybe String
  -> FileDTO
  -> ServerContextM (Headers '[Header "x-trace-uuid" String] DocumentTemplateSimple)
list_bundle_POST mTokenHeader reqDto =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader
        =<< importBundle reqDto.content
