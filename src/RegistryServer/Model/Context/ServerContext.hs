module RegistryServer.Model.Context.ServerContext where

import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (LoggingT, MonadLogger)
import Control.Monad.Reader (MonadReader, ReaderT)
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Network.HTTP.Client (Manager)
import Network.Minio (MinioConn)
import Servant (ServerError)

import RegistryServer.Model.Config.ServerConfig
import Shared.Model.Config.BuildInfoConfig

data ServerContext = ServerContext
  { serverConfig :: ServerConfig
  , buildInfoConfig :: BuildInfoConfig
  , dbPool :: Pool Connection
  , s3Client :: MinioConn
  , httpClientManager :: Manager
  }

newtype ServerContextM a = ServerContextM {runServerContextM :: ReaderT ServerContext (LoggingT (ExceptT ServerError IO)) a}
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader ServerContext, MonadError ServerError, MonadLogger)
