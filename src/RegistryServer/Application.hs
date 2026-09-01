module RegistryServer.Application where

import Control.Concurrent (MVar)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (MonadLogger)
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Network.HTTP.Client (Manager)
import Network.Minio (MinioConn)

import RegistryServer.Api.Middleware.LoggingMiddleware
import RegistryServer.Api.Sentry
import RegistryServer.Api.Web
import RegistryServer.Constant.ASCIIArt
import RegistryServer.Constant.Resource
import qualified RegistryServer.Database.Migration.Development.Migration as DevDB
import qualified RegistryServer.Database.Migration.Production.Migration as ProdDB
import RegistryServer.Model.Config.ServerConfig
import RegistryServer.Model.Config.ServerConfigIM ()
import RegistryServer.Model.Config.ServerConfigJM ()
import RegistryServer.Model.Context.ContextMappers
import RegistryServer.Model.Context.ServerContext
import RegistryServer.Service.Config.Server.ServerConfigValidation
import RegistryServer.Worker.CronWorkers
import RegistryServer.Worker.PermanentWorkers
import Shared.Application
import Shared.Bootstrap.Web
import Shared.Bootstrap.Worker
import Shared.Model.Config.BuildInfoConfig
import Shared.Model.Config.ServerConfig

runApplication :: IO ()
runApplication =
  runWebServerWithWorkers
    [putStrLn asciiLogo]
    serverConfigFile
    validateServerConfig
    buildInfoFile
    createServerContext
    ProdDB.migrationDefinitions
    DevDB.runMigration
    afterDbMigrationHook
    runWebServer
    runWorker

createServerContext :: (MonadIO m, MonadLogger m) => ServerConfig -> BuildInfoConfig -> Pool Connection -> MinioConn -> Manager -> MVar () -> m ServerContext
createServerContext serverConfig buildInfoConfig dbPool s3Client httpClientManager shutdownFlag = return ServerContext {..}

afterDbMigrationHook :: ServerContext -> IO ()
afterDbMigrationHook _ = return ()

runWebServer :: ServerContext -> IO ()
runWebServer context = runWebServerFactory context getSentryIdentity loggingMiddleware webApi webServer

runWorker :: MVar () -> ServerContext -> IO ()
runWorker shutdownFlag context =
  worker runRequestContextWithServerContext runRequestContextWithServerContext'' shutdownFlag context workers permanentWorker
