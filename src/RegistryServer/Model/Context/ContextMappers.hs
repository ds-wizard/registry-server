module RegistryServer.Model.Context.ContextMappers where

import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (liftIO, runReaderT)
import Data.IORef (newIORef)
import Data.Pool
import Data.Time

import RegistryServer.Database.Migration.Development.User.Data.Users
import RegistryServer.Model.Config.ServerConfig
import RegistryServer.Model.Context.RequestContext
import RegistryServer.Model.Context.ServerContext
import Shared.Model.Config.ServerConfig
import Shared.Model.Context.TransactionState
import Shared.Util.Logger
import Shared.Util.Uuid

runRequestContextWithServerContext :: RequestContextM a -> ServerContext -> IO (Either String a)
runRequestContextWithServerContext function serverContext =
  requestContextFromServerContext (Just userAdmin) Transactional serverContext $
    runRequestContextWithRequestContext function

runRequestContextWithServerContext'' :: RequestContextM a -> ServerContext -> IO (Either String a)
runRequestContextWithServerContext'' function serverContext =
  requestContextFromServerContext (Just userAdmin) NoTransaction serverContext $
    runRequestContextWithRequestContext function

runRequestContextWithRequestContext :: RequestContextM a -> RequestContext -> IO (Either String a)
runRequestContextWithRequestContext function requestContext = do
  eResult <- liftIO $ runMonads function.runRequestContextM requestContext
  case eResult of
    Right result -> return . Right $ result
    Left error ->
      runLogging' requestContext $ do
        logError _CMP_SERVER ("Caught error: " ++ show error)
        return . Left $ show error

runRequestContextWithRequestContext' :: RequestContextM a -> RequestContext -> IO (Either String a)
runRequestContextWithRequestContext' function requestContext =
  withResource requestContext.dbPool $ \dbConn -> do
    let updatedRequestContext = requestContext {dbConnection = Just dbConn}
    eResult <- liftIO $ runMonads function.runRequestContextM updatedRequestContext
    case eResult of
      Right result -> return . Right $ result
      Left error ->
        runLogging' updatedRequestContext $ do
          logError _CMP_SERVER ("Caught error: " ++ show error)
          return . Left $ show error

runMonads fn context = runExceptT $ runLogging' context $ runReaderT fn context

runLogging' context =
  let loggingLevel = context.serverConfig.logging.level
   in runLogging loggingLevel

requestContextFromServerContext currentUser transactionState serverContext callback = do
  cTraceUuid <- generateUuid
  cBreadcrumbs <- liftIO (newIORef [])
  now <- liftIO getCurrentTime
  let requestContext =
        RequestContext
          { serverConfig = serverContext.serverConfig
          , buildInfoConfig = serverContext.buildInfoConfig
          , dbPool = serverContext.dbPool
          , dbConnection = Nothing
          , s3Client = serverContext.s3Client
          , httpClientManager = serverContext.httpClientManager
          , traceUuid = cTraceUuid
          , breadcrumbs = cBreadcrumbs
          , currentUser = currentUser
          }
  case transactionState of
    Transactional -> do
      withResource serverContext.dbPool $ \dbConn -> do
        let requestContextWithConn = requestContext {dbConnection = Just dbConn}
        callback requestContextWithConn
    NoTransaction -> do
      callback requestContext
