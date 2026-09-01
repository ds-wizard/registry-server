module RegistryServer.Api.Handler.KnowledgeModelPackage.Detail_GET where

import Servant

import RegistryServer.Api.Handler.Common
import RegistryServer.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageDetailDTO
import RegistryServer.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageDetailJM ()
import RegistryServer.Model.Context.ContextLenses ()
import RegistryServer.Model.Context.ServerContext
import RegistryServer.Service.KnowledgeModel.Package.KnowledgeModelPackageService
import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState
import Shared.Model.Coordinate.Coordinate

type Detail_GET =
  "knowledge-model-packages"
    :> Capture "coordinate" Coordinate
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] KnowledgeModelPackageDetailDTO)

detail_GET :: Coordinate -> ServerContextM (Headers '[Header "x-trace-uuid" String] KnowledgeModelPackageDetailDTO)
detail_GET coordinate = runInUnauthService NoTransaction $ addTraceUuidHeader =<< getPackageByCoordinate coordinate
