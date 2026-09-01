module RegistryServer.Api.Handler.KnowledgeModelPackage.List_GET where

import Data.Maybe (catMaybes)
import Servant

import RegistryPublic.Api.Resource.Package.KnowledgeModelPackageSimpleDTO
import RegistryPublic.Api.Resource.Package.KnowledgeModelPackageSimpleJM ()
import RegistryServer.Api.Handler.Common
import RegistryServer.Model.Context.ContextLenses ()
import RegistryServer.Model.Context.ServerContext
import RegistryServer.Service.KnowledgeModel.Package.KnowledgeModelPackageService
import Shared.Api.Handler.Common
import Shared.Constant.Api
import Shared.Model.Context.TransactionState

list_GET
  :: Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe Int
  -> ServerContextM (Headers '[Header "x-trace-uuid" String] [KnowledgeModelPackageSimpleDTO])
list_GET mTokenHeader xUserCountHeaderValue xPkgCountHeaderValue xProjectCountHeaderValue xKnowledgeModelEditorCountHeaderValue xDocCountHeaderValue xTmlCountHeaderValue mOrganizationId mKmId mMetamodelVersion =
  getMaybeAuthServiceExecutor mTokenHeader $ \runInMaybeAuthService ->
    runInMaybeAuthService Transactional $
      addTraceUuidHeader =<< do
        let queryParams = catMaybes [(,) "organization_id" <$> mOrganizationId, (,) "km_id" <$> mKmId]
        let headers =
              catMaybes
                [ (,) xUserCountHeaderName <$> xUserCountHeaderValue
                , (,) xKnowledgeModelPackageCountHeaderName <$> xPkgCountHeaderValue
                , (,) xProjectCountHeaderName <$> xProjectCountHeaderValue
                , (,) xKnowledgeModelEditorCountHeaderName <$> xKnowledgeModelEditorCountHeaderValue
                , (,) xDocCountHeaderName <$> xDocCountHeaderValue
                , (,) xTmlCountHeaderName <$> xTmlCountHeaderValue
                ]
        getSimplePackagesFiltered queryParams mMetamodelVersion headers
