module RegistryServer.Api.Handler.KnowledgeModelPackage.List_Bundle_POST where

import Servant

import RegistryServer.Api.Handler.Common
import RegistryServer.Model.Context.ContextLenses ()
import RegistryServer.Model.Context.ServerContext
import RegistryServer.Service.KnowledgeModel.Bundle.KnowledgeModelBundleService
import Shared.Api.Handler.Common
import Shared.Api.Resource.KnowledgeModel.Bundle.KnowledgeModelBundleJM ()
import Shared.Model.Context.TransactionState
import Shared.Model.KnowledgeModel.Bundle.KnowledgeModelBundle

list_bundle_POST
  :: Maybe String
  -> KnowledgeModelBundle
  -> ServerContextM (Headers '[Header "x-trace-uuid" String] KnowledgeModelBundle)
list_bundle_POST mTokenHeader reqDto =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< importBundle reqDto
