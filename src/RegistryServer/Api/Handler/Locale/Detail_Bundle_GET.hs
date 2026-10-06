module RegistryServer.Api.Handler.Locale.Detail_Bundle_GET where

import Control.Monad.Reader (asks)
import qualified Data.UUID as U
import Servant

import RegistryServer.Api.Handler.Common
import RegistryServer.Model.Context.RequestContext
import RegistryServer.Model.Context.ServerContext
import RegistryServer.Service.Locale.Bundle.LocaleBundleService
import Shared.Api.Handler.Common
import Shared.Api.Resource.Coordinate.CoordinateJM ()
import Shared.Model.Context.TransactionState
import Shared.Model.Coordinate.Coordinate

type Detail_Bundle_GET =
  Header "Authorization" String
    :> "locales"
    :> Capture "coordinate" Coordinate
    :> "bundle"
    :> Get '[OctetStream] (Headers '[Header "x-trace-uuid" String, Header "Content-Disposition" String] FileStreamLazy)

detail_bundle_GET
  :: Maybe String
  -> Coordinate
  -> ServerContextM (Headers '[Header "x-trace-uuid" String, Header "Content-Disposition" String] FileStreamLazy)
detail_bundle_GET mTokenHeader coordinate =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService NoTransaction $ do
      zipFile <- exportBundle coordinate
      let cdHeader = "attachment;filename=\"locale.zip\""
      traceUuid <- asks (.traceUuid)
      return . addHeader (U.toString traceUuid) . addHeader cdHeader . FileStreamLazy $ zipFile
