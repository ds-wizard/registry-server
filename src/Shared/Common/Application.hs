module Shared.Common.Application where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad.Reader (liftIO)
import Data.Foldable (forM_)
import System.Exit
import System.IO

import Shared.Common.Bootstrap.AwsAppConfig
import Shared.Common.Bootstrap.Config
import Shared.Common.Bootstrap.DatabaseMigration
import Shared.Common.Bootstrap.HttpClient
import Shared.Common.Bootstrap.Postgres
import Shared.Common.Bootstrap.S3
import Shared.Common.Constant.Component
import Shared.Common.Model.Config.ServerConfig
import Shared.Common.Service.Config.BuildInfo.BuildInfoConfigService
import Shared.Common.Service.Config.Server.ServerConfigService
import Shared.Common.Util.Logger

runWebServerWithWorkers
  beforeLoadActions
  serverConfigFile
  validateServerConfig
  buildInfoFile
  createBaseContext
  prodDBMigrations
  runDevDBMigrations
  afterDbMigrationHook
  runWebServer
  worker =
    do
      hSetBuffering stdout LineBuffering
      sequence_ beforeLoadActions
      shutdownFlag <- newEmptyMVar
      (configBytes, pollForChanges) <- resolveConfigBytes serverConfigFile
      configValue <- loadConfigValue serverConfigFile configBytes
      _ <- forkIO $ pollForChanges shutdownFlag
      baseContext <-
        setupApp
          Nothing
          Nothing
          serverConfigFile
          configValue
          validateServerConfig
          buildInfoFile
          createBaseContext
          prodDBMigrations
          runDevDBMigrations
          afterDbMigrationHook
          shutdownFlag
      race_ (takeMVar shutdownFlag) (concurrently (runWebServer baseContext) (worker shutdownFlag baseContext))

-- Builds one application's BaseContext from an already loaded config and runs its migrations.
-- The shutdownFlag is passed in so several applications hosted in one process can share it,
-- mDbPool and mS3Client so they can share one connection pool over one database and one client
-- over one bucket, and configValue so they can be projections of one config file.
setupApp
  mDbPool
  mS3Client
  configLabel
  configValue
  validateServerConfig
  buildInfoFile
  createBaseContext
  prodDBMigrations
  runDevDBMigrations
  afterDbMigrationHook
  shutdownFlag =
    do
      serverConfig <- loadConfigWith configLabel configValue (getServerConfigFromValue validateServerConfig)
      buildInfoConfig <- loadConfig buildInfoFile getBuildInfoConfig
      runLogging serverConfig.logging.level $ do
        logInfo _CMP_ENVIRONMENT $ "set to " ++ serverConfig.general.environment
        dbPool <-
          case mDbPool of
            Just pool -> do
              logInfo _CMP_DATABASE "reusing the shared postgres connection pool"
              return pool
            Nothing -> connectPostgresDB serverConfig.logging serverConfig.database
        httpClientManager <- setupHttpClientManager serverConfig.logging
        s3Client <-
          case mS3Client of
            Just client -> do
              logInfo _CMP_S3 "reusing the shared S3 client"
              return client
            Nothing -> setupS3Client serverConfig.s3 httpClientManager
        baseContext <- createBaseContext serverConfig buildInfoConfig dbPool s3Client httpClientManager shutdownFlag
        result <- liftIO $ runDBMigration baseContext prodDBMigrations runDevDBMigrations
        forM_ result (liftIO . die)
        liftIO $ afterDbMigrationHook baseContext
        return baseContext
