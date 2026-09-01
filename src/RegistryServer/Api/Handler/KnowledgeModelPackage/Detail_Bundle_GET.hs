module RegistryServer.Api.Handler.KnowledgeModelPackage.Detail_Bundle_GET where

import Servant

import RegistryServer.Api.Handler.Common
import RegistryServer.Api.Resource.KnowledgeModel.Bundle.KnowledgeModelBundleJM ()
import RegistryServer.Model.Context.ContextLenses ()
import RegistryServer.Model.Context.ServerContext
import RegistryServer.Model.KnowledgeModel.Bundle.KnowledgeModelBundle
import RegistryServer.Service.KnowledgeModel.Bundle.KnowledgeModelBundleService
import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState
import Shared.Model.Coordinate.Coordinate

type Detail_Bundle_GET =
  Header "Authorization" String
    :> "knowledge-model-packages"
    :> Capture "coordinate" Coordinate
    :> "bundle"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] KnowledgeModelBundle)

detail_bundle_GET :: Maybe String -> Coordinate -> ServerContextM (Headers '[Header "x-trace-uuid" String] KnowledgeModelBundle)
detail_bundle_GET mTokenHeader kmpId =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< exportBundle kmpId
