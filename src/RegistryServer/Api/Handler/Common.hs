module RegistryServer.Api.Handler.Common where

import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (ask, liftIO, runReaderT)
import Data.IORef (newIORef)
import Data.Pool
import Servant (throwError)

import RegistryPublic.Api.Resource.Package.KnowledgeModelPackageSimpleJM ()
import RegistryPublic.Model.Organization.Organization
import RegistryServer.Database.DAO.Organization.OrganizationDAO
import RegistryServer.Model.Config.ServerConfig
import qualified RegistryServer.Model.Context.RequestContext as RequestContext
import RegistryServer.Model.Context.ServerContext
import Shared.Api.Handler.Common
import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Localization.Messages.Public
import Shared.Model.Config.ServerConfig
import Shared.Model.Context.TransactionState
import Shared.Model.Error.Error
import Shared.Service.Sentry.SentryService
import Shared.Util.Logger
import Shared.Util.Token
import Shared.Util.Uuid

runInUnauthService :: TransactionState -> RequestContext.RequestContextM a -> ServerContextM a
runInUnauthService = runIn Nothing

runInAuthService :: Organization -> TransactionState -> RequestContext.RequestContextM a -> ServerContextM a
runInAuthService org = runIn (Just org)

runIn :: Maybe Organization -> TransactionState -> RequestContext.RequestContextM a -> ServerContextM a
runIn mOrganization transactionState function = do
  serverContext <- ask
  traceUuid <- liftIO generateUuid
  breadcrumbs <- liftIO (newIORef [])
  let requestContext =
        RequestContext.RequestContext
          { serverConfig = serverContext.serverConfig
          , buildInfoConfig = serverContext.buildInfoConfig
          , dbPool = serverContext.dbPool
          , dbConnection = Nothing
          , s3Client = serverContext.s3Client
          , httpClientManager = serverContext.httpClientManager
          , traceUuid = traceUuid
          , breadcrumbs = breadcrumbs
          , currentOrganization = mOrganization
          }
  let loggingLevel = serverContext.serverConfig.logging.level
  eResult <-
    case transactionState of
      Transactional ->
        liftIO $ withResource serverContext.dbPool $ \dbConn ->
          let transactionContext = requestContext {RequestContext.dbConnection = Just dbConn}
           in guardRequestContext transactionContext (runExceptT $ runLogging loggingLevel $ runReaderT (RequestContext.runRequestContextM function) transactionContext)
      NoTransaction -> liftIO $ guardRequestContext requestContext (runExceptT $ runLogging loggingLevel $ runReaderT (RequestContext.runRequestContextM function) requestContext)
  case eResult of
    Right result -> return result
    Left error -> throwError =<< sendError error

getMaybeAuthServiceExecutor :: Maybe String -> ((TransactionState -> RequestContext.RequestContextM a -> ServerContextM a) -> ServerContextM b) -> ServerContextM b
getMaybeAuthServiceExecutor (Just tokenHeader) callback = do
  organization <- getCurrentOrganization tokenHeader
  callback (runInAuthService organization)
getMaybeAuthServiceExecutor Nothing callback = callback runInUnauthService

getAuthServiceExecutor :: Maybe String -> ((TransactionState -> RequestContext.RequestContextM a -> ServerContextM a) -> ServerContextM b) -> ServerContextM b
getAuthServiceExecutor (Just token) callback = do
  org <- getCurrentOrganization token
  callback (runInAuthService org)
getAuthServiceExecutor Nothing _ = throwError =<< (sendError . UnauthorizedError $ _ERROR_API_COMMON__UNABLE_TO_GET_TOKEN)

getCurrentOrganization :: String -> ServerContextM Organization
getCurrentOrganization tokenHeader = do
  orgToken <- getCurrentOrgToken tokenHeader
  mOrg <- runInUnauthService NoTransaction (findOrganizationByToken' orgToken)
  case mOrg of
    Just org -> return org
    Nothing -> throwError =<< (sendError . UnauthorizedError $ _ERROR_API_COMMON__UNABLE_TO_GET_ORGANIZATION)

getCurrentOrgToken :: String -> ServerContextM String
getCurrentOrgToken tokenHeader =
  case separateToken tokenHeader of
    Just orgToken -> return orgToken
    Nothing -> throwError =<< (sendError . UnauthorizedError $ _ERROR_API_COMMON__UNABLE_TO_GET_TOKEN)
