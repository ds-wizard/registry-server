module RegistryServer.Model.Context.ContextLenses where

import Data.IORef (IORef)
import Data.Pool (Pool)
import qualified Data.UUID as U
import Database.PostgreSQL.Simple (Connection)
import GHC.Records
import Network.HTTP.Client (Manager)
import Network.Minio (MinioConn)

import RegistryServer.Model.Config.ServerConfig
import RegistryServer.Model.Context.RequestContext
import RegistryServer.Model.Context.ServerContext
import RegistryServer.Model.User.User
import Shared.Constant.Tenant
import Shared.Model.Config.BuildInfoConfig
import Shared.Model.Config.ServerConfig
import Shared.Model.Config.ServerConfigDM
import qualified Shared.Model.Context.RequestContext as S_RequestContext
import qualified Shared.Model.Context.ServerContext as S_ServerContext
import Shared.Model.Sentry.SentryEvent
import Shared.Service.Acl.AclService

instance S_ServerContext.ServerContextType ServerContext ServerConfig

instance S_ServerContext.ServerContextC ServerContext ServerConfig ServerContextM

instance S_RequestContext.RequestContextType RequestContext ServerConfig

instance S_RequestContext.RequestContextC RequestContext ServerConfig RequestContextM

instance HasField "serverConfig'" RequestContext ServerConfig where
  getField = (.serverConfig)

instance HasField "serverConfig'" ServerContext ServerConfig where
  getField = (.serverConfig)

instance HasField "serverPort'" ServerConfig Int where
  getField = (.general.serverPort)

instance HasField "environment'" ServerConfig String where
  getField = (.general.environment)

instance HasField "database'" ServerConfig ServerConfigDatabase where
  getField = (.database)

instance HasField "s3'" ServerConfig ServerConfigS3 where
  getField = (.s3)

instance HasField "sentry'" ServerConfig ServerConfigSentry where
  getField = (.sentry)

instance HasField "logging'" ServerConfig ServerConfigLogging where
  getField = (.logging)

instance HasField "cloud'" ServerConfig ServerConfigCloud where
  getField = (.cloud)

instance HasField "persistentCommand'" ServerConfig ServerConfigPersistentCommand where
  getField = (.persistentCommand)

instance HasField "aws'" ServerConfig ServerConfigAws where
  getField _ = defaultAws

instance HasField "dbPool'" RequestContext (Pool Connection) where
  getField = (.dbPool)

instance HasField "dbPool'" ServerContext (Pool Connection) where
  getField = (.dbPool)

instance HasField "dbConnection'" RequestContext (Maybe Connection) where
  getField = (.dbConnection)

instance HasField "s3Client'" RequestContext MinioConn where
  getField = (.s3Client)

instance HasField "s3Client'" ServerContext MinioConn where
  getField = (.s3Client)

instance HasField "httpClientManager'" RequestContext Manager where
  getField = (.httpClientManager)

instance HasField "httpClientManager'" ServerContext Manager where
  getField = (.httpClientManager)

instance HasField "buildInfoConfig'" RequestContext BuildInfoConfig where
  getField = (.buildInfoConfig)

instance HasField "buildInfoConfig'" ServerContext BuildInfoConfig where
  getField = (.buildInfoConfig)

instance HasField "identity'" RequestContext (Maybe String) where
  getField entity = fmap (U.toString . (.uuid)) entity.currentUser

instance HasField "identityEmail'" RequestContext (Maybe String) where
  getField entity = fmap (.email) entity.currentUser

instance HasField "traceUuid'" RequestContext U.UUID where
  getField = (.traceUuid)

instance HasField "breadcrumbs'" RequestContext (IORef [SentryBreadcrumb]) where
  getField = (.breadcrumbs)

instance HasField "tenantUuid'" RequestContext U.UUID where
  getField entity = defaultTenantUuid

instance AclContext RequestContextM where
  checkPermission perm = return ()
  checkPermissionsAny perms = return ()
  checkPermissionsAll perms = return ()
  hasPermission perm = return True
  hasPermissionInWorkspace perm _ = return True
  checkPermissionInWorkspace perm _ = return ()
