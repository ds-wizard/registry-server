module RegistryServer.Model.Context.RequestContext where

import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (LoggingT, MonadLogger)
import Control.Monad.Reader (MonadReader, ReaderT)
import Data.IORef (IORef)
import Data.Pool (Pool)
import qualified Data.UUID as U
import Database.PostgreSQL.Simple (Connection)
import Network.HTTP.Client (Manager)
import Network.Minio (MinioConn)

import RegistryServer.Model.Config.ServerConfig
import RegistryServer.Model.User.User
import Shared.Model.Config.BuildInfoConfig
import Shared.Model.Error.Error
import Shared.Model.Sentry.SentryEvent

data RequestContext = RequestContext
  { serverConfig :: ServerConfig
  , buildInfoConfig :: BuildInfoConfig
  , dbPool :: Pool Connection
  , dbConnection :: Maybe Connection
  , s3Client :: MinioConn
  , httpClientManager :: Manager
  , traceUuid :: U.UUID
  , breadcrumbs :: IORef [SentryBreadcrumb]
  , currentUser :: Maybe User
  }

newtype RequestContextM a = RequestContextM {runRequestContextM :: ReaderT RequestContext (LoggingT (ExceptT AppError IO)) a}
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader RequestContext, MonadError AppError, MonadLogger)
