module RegistryServer.Api.Handler.DocumentTemplate.List_GET where

import Data.Maybe (maybeToList)
import Servant

import RegistryPublic.Api.Resource.DocumentTemplate.DocumentTemplateSimpleDTO
import RegistryPublic.Api.Resource.DocumentTemplate.DocumentTemplateSimpleJM ()
import RegistryServer.Api.Handler.Common
import RegistryServer.Model.Context.ContextLenses ()
import RegistryServer.Model.Context.ServerContext
import RegistryServer.Service.DocumentTemplate.DocumentTemplateService
import Shared.Api.Handler.Common
import Shared.Api.Resource.Common.SemVer2TupleJM ()
import Shared.Model.Common.SemVer2Tuple
import Shared.Model.Context.TransactionState

list_GET
  :: Maybe String
  -> Maybe String
  -> Maybe SemVer2Tuple
  -> ServerContextM (Headers '[Header "x-trace-uuid" String] [DocumentTemplateSimpleDTO])
list_GET mTokenHeader mId mMetamodelVersion =
  getMaybeAuthServiceExecutor mTokenHeader $ \runInMaybeAuthService ->
    runInMaybeAuthService NoTransaction $
      addTraceUuidHeader =<< do
        let queryParams = maybeToList ((,) "id" <$> mId)
        getDocumentTemplates queryParams mMetamodelVersion
