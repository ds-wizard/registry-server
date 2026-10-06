module RegistryServer.Api.Handler.Common where

import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (ask, liftIO, runReaderT)
import Data.IORef (newIORef)
import Data.Pool
import Servant (throwError)

import RegistryPublic.Api.Resource.Package.KnowledgeModelPackageSimpleJM ()
import RegistryServer.Database.DAO.User.UserDAO
import RegistryServer.Localization.Messages.Public
import RegistryServer.Model.Config.ServerConfig
import qualified RegistryServer.Model.Context.RequestContext as RequestContext
import RegistryServer.Model.Context.ServerContext
import RegistryServer.Model.User.User
import Shared.Api.Handler.Common
import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Localization.Messages.Public
import Shared.Model.Config.ServerConfig
import Shared.Model.Context.TransactionState
import Shared.Model.Error.Error
import Shared.Service.Sentry.SentryService
import Shared.Util.Crypto (hashSHA256)
import Shared.Util.Logger
import Shared.Util.Token
import Shared.Util.Uuid

runInUnauthService :: TransactionState -> RequestContext.RequestContextM a -> ServerContextM a
runInUnauthService = runIn Nothing

runInAuthService :: User -> TransactionState -> RequestContext.RequestContextM a -> ServerContextM a
runInAuthService user = runIn (Just user)

runIn :: Maybe User -> TransactionState -> RequestContext.RequestContextM a -> ServerContextM a
runIn mUser transactionState function = do
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
          , currentUser = mUser
          }
  let loggingLevel = serverContext.serverConfig.logging.level
  eResult <-
    case transactionState of
      Transactional ->
        liftIO $ withResource serverContext.dbPool $ \dbConn ->
          let transactionContext = requestContext {RequestContext.dbConnection = Just dbConn}
           in guardRequestContext transactionContext (runExceptT $ runLogging loggingLevel $ runReaderT function.runRequestContextM transactionContext)
      NoTransaction -> liftIO $ guardRequestContext requestContext (runExceptT $ runLogging loggingLevel $ runReaderT function.runRequestContextM requestContext)
  case eResult of
    Right result -> return result
    Left error -> throwError =<< sendError error

getMaybeAuthServiceExecutor :: Maybe String -> ((TransactionState -> RequestContext.RequestContextM a -> ServerContextM a) -> ServerContextM b) -> ServerContextM b
getMaybeAuthServiceExecutor (Just tokenHeader) callback = do
  user <- getCurrentUser tokenHeader
  callback (runInAuthService user)
getMaybeAuthServiceExecutor Nothing callback = callback runInUnauthService

getAuthServiceExecutor :: Maybe String -> ((TransactionState -> RequestContext.RequestContextM a -> ServerContextM a) -> ServerContextM b) -> ServerContextM b
getAuthServiceExecutor (Just token) callback = do
  user <- getCurrentUser token
  callback (runInAuthService user)
getAuthServiceExecutor Nothing _ = throwError =<< (sendError . UnauthorizedError $ _ERROR_API_COMMON__UNABLE_TO_GET_TOKEN)

getCurrentUser :: String -> ServerContextM User
getCurrentUser tokenHeader = do
  token <- getCurrentToken tokenHeader
  mUser <- runInUnauthService NoTransaction (findUserByTokenHash' (hashSHA256 token))
  case mUser of
    Just user | user.active -> return user
    Just _ -> throwError =<< (sendError . UnauthorizedError $ _ERROR_SERVICE_TOKEN__ACCOUNT_IS_NOT_ACTIVATED)
    Nothing -> throwError =<< (sendError . UnauthorizedError $ _ERROR_API_COMMON__UNABLE_TO_GET_USER)

getCurrentToken :: String -> ServerContextM String
getCurrentToken tokenHeader =
  case separateToken tokenHeader of
    Just token -> return token
    Nothing -> throwError =<< (sendError . UnauthorizedError $ _ERROR_API_COMMON__UNABLE_TO_GET_TOKEN)
