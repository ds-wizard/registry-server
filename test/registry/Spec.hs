module Main where

import Control.Monad ((>=>))
import qualified Data.ByteString as BS
import Data.IORef (newIORef)
import Data.Maybe (fromJust)
import Data.Pool
import qualified Data.UUID as U
import Test.Hspec

import RegistryServer.Constant.Resource
import RegistryServer.Database.Migration.Development.User.Data.Users
import RegistryServer.Model.Config.ServerConfig
import RegistryServer.Model.Config.ServerConfigIM ()
import RegistryServer.Model.Config.ServerConfigJM ()
import RegistryServer.Model.Context.RequestContext
import RegistryServer.Model.Context.ServerContext
import RegistryServer.Service.Config.Server.ServerConfigValidation
import Shared.Database.Connection
import Shared.Integration.Http.Common.HttpClientFactory
import Shared.Model.Config.ServerConfig
import Shared.S3.Common
import Shared.Service.Config.BuildInfo.BuildInfoConfigService
import Shared.Service.Config.Server.ServerConfigService

import Specs.Api.Handler.ApiKey.ApiSpec
import Specs.Api.Handler.Bootstrap.ApiSpec
import Specs.Api.Handler.DocumentTemplate.ApiSpec
import Specs.Api.Handler.Info.ApiSpec
import Specs.Api.Handler.KnowledgeModelPackage.ApiSpec
import Specs.Api.Handler.Locale.ApiSpec
import Specs.Api.Handler.Token.ApiSpec
import Specs.Api.Handler.User.ApiSpec
import Specs.Api.Handler.UserEmailLink.ApiSpec
import Specs.Database.Migration.Production.Migration_5_0_0.MigrationSpec
import Specs.Service.KnowledgeModel.Package.PackageValidationSpec
import TestMigration

hLoadConfig fileName loadFn callback = do
  eitherConfig <- loadFn fileName
  case eitherConfig of
    Left error -> do
      putStrLn $ "CONFIG: load failed (" ++ fileName ++ ")"
      putStrLn $ "CONFIG: can't load " ++ fileName ++ ". Maybe the file is missing or not well-formatted"
      putStrLn $ "CONFIG: " ++ show error
    Right config -> do
      putStrLn $ "CONFIG: '" ++ fileName ++ "' loaded"
      callback config

prepareWebApp runCallback =
  hLoadConfig serverConfigFileTest (BS.readFile >=> getServerConfig validateServerConfig) $ \serverConfig ->
    hLoadConfig buildInfoConfigFileTest getBuildInfoConfig $ \buildInfoConfig -> do
      putStrLn $ "ENVIRONMENT: set to " `mappend` serverConfig.general.environment
      dbPool <- createDatabaseConnectionPool serverConfig.database
      putStrLn "DATABASE: connected"
      httpClientManager <- createHttpClientManager serverConfig.logging
      putStrLn "HTTP_CLIENT: created"
      s3Client <- createS3Client serverConfig.s3 httpClientManager
      putStrLn "S3_CLIENT: created"
      let serverContext =
            ServerContext
              { serverConfig = serverConfig
              , buildInfoConfig = buildInfoConfig
              , dbPool = dbPool
              , s3Client = s3Client
              , httpClientManager = httpClientManager
              }
      withResource dbPool $ \dbConnection -> do
        breadcrumbs <- newIORef []
        let requestContext =
              RequestContext
                { serverConfig = serverConfig
                , buildInfoConfig = buildInfoConfig
                , dbPool = dbPool
                , dbConnection = Just dbConnection
                , s3Client = s3Client
                , httpClientManager = httpClientManager
                , traceUuid = fromJust (U.fromString "2ed6eb01-e75e-4c63-9d81-7f36d84192c0")
                , breadcrumbs = breadcrumbs
                , currentUser = Just userAdmin
                }
        buildSchema requestContext
        runCallback serverContext requestContext

main :: IO ()
main =
  prepareWebApp
    ( \serverContext requestContext ->
        hspec $ do
          describe "UNIT TESTING" $ describe "SERVICE" $ do
            describe "KnowledgeModel" $
              describe
                "Package"
                packageValidationSpec
          before (resetDB requestContext) $ describe "INTEGRATION TESTING" $ describe "API" $ do
            userEmailLinkAPI serverContext requestContext
            apiKeyAPI serverContext requestContext
            bootstrapAPI serverContext requestContext
            infoAPI serverContext requestContext
            knowledgeModelPackageAPI serverContext requestContext
            localeAPI serverContext requestContext
            templateAPI serverContext requestContext
            tokenAPI serverContext requestContext
            userAPI serverContext requestContext
          before (resetDB requestContext) $
            describe "INTEGRATION TESTING" $
              describe "MIGRATION" $
                migration_5_0_0Spec requestContext
    )
