module RegistryServer.Api.Handler.DocumentTemplate.List_GET where

import Data.Maybe (catMaybes)
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
  -> Maybe String
  -> Maybe SemVer2Tuple
  -> ServerContextM (Headers '[Header "x-trace-uuid" String] [DocumentTemplateSimpleDTO])
list_GET mTokenHeader mOrganizationId mTmlId mMetamodelVersion =
  getMaybeAuthServiceExecutor mTokenHeader $ \runInMaybeAuthService ->
    runInMaybeAuthService NoTransaction $
      addTraceUuidHeader =<< do
        let queryParams = catMaybes [(,) "organization_id" <$> mOrganizationId, (,) "template_id" <$> mTmlId]
        getDocumentTemplates queryParams mMetamodelVersion
