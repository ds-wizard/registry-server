module RegistryServer.Worker.PermanentWorkers where

import Control.Concurrent
import Control.Monad (when)
import Control.Monad.Logger (LoggingT)
import Control.Monad.Reader (liftIO)
import Prelude hiding (log)

import RegistryServer.Model.Config.ServerConfig
import RegistryServer.Model.Context.ContextMappers
import RegistryServer.Model.Context.ServerContext
import RegistryServer.Service.PersistentCommand.PersistentCommandService
import Shared.Model.Config.ServerConfig
import Shared.Util.Logger

permanentWorker :: ServerContext -> LoggingT IO [ThreadId]
permanentWorker context = do
  threadId <- liftIO $ forkIO (persistentCommandListenerJob context)
  return [threadId]

-- -----------------------------------------------------------------------------
-- WORKERS
-- -----------------------------------------------------------------------------
persistentCommandListenerJob :: ServerContext -> IO ()
persistentCommandListenerJob context =
  when
    context.serverConfig.persistentCommand.listenerJob.enabled
    ( do
        let loggingLevel = context.serverConfig.logging.level
         in runLogging loggingLevel $ do
              logInfo _CMP_WORKER "PersistentCommandWorker: starting"
              liftIO $ runRequestContextWithServerContext runPersistentCommandChannelListener' context
              logInfo _CMP_WORKER "PersistentCommandWorker: ended"
    )
