module RegistryServer.Api.Handler.Locale.Detail_GET where

import Servant

import RegistryServer.Api.Handler.Common
import RegistryServer.Api.Resource.Locale.LocaleDetailDTO
import RegistryServer.Api.Resource.Locale.LocaleDetailJM ()
import RegistryServer.Model.Context.ContextLenses ()
import RegistryServer.Model.Context.ServerContext
import RegistryServer.Service.Locale.LocaleService
import Shared.Api.Handler.Common
import Shared.Api.Resource.Coordinate.CoordinateJM ()
import Shared.Model.Context.TransactionState
import Shared.Model.Coordinate.Coordinate

type Detail_GET =
  "locales"
    :> Capture "coordinate" Coordinate
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] LocaleDetailDTO)

detail_GET :: Coordinate -> ServerContextM (Headers '[Header "x-trace-uuid" String] LocaleDetailDTO)
detail_GET coordinate = runInUnauthService NoTransaction $ addTraceUuidHeader =<< getLocaleByCoordinate coordinate
