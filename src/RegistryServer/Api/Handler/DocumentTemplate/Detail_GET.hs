module RegistryServer.Api.Handler.DocumentTemplate.Detail_GET where

import Servant

import RegistryServer.Api.Handler.Common
import RegistryServer.Api.Resource.DocumentTemplate.DocumentTemplateDetailDTO
import RegistryServer.Api.Resource.DocumentTemplate.DocumentTemplateDetailJM ()
import RegistryServer.Model.Context.ContextLenses ()
import RegistryServer.Model.Context.ServerContext
import RegistryServer.Service.DocumentTemplate.DocumentTemplateService
import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState
import Shared.Model.Coordinate.Coordinate

type Detail_GET =
  "document-templates"
    :> Capture "coordinate" Coordinate
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] DocumentTemplateDetailDTO)

detail_GET :: Coordinate -> ServerContextM (Headers '[Header "x-trace-uuid" String] DocumentTemplateDetailDTO)
detail_GET coordinate = runInUnauthService NoTransaction $ addTraceUuidHeader =<< getDocumentTemplateByCoordinate coordinate
