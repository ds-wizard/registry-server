module RegistryServer.Worker.CronWorkers where

import RegistryServer.Model.Config.ServerConfig
import RegistryServer.Model.Context.ContextLenses ()
import RegistryServer.Model.Context.RequestContext
import RegistryServer.Model.Context.ServerContext
import RegistryServer.Service.PersistentCommand.PersistentCommandService
import Shared.Model.Config.ServerConfig
import Shared.Model.Worker.CronWorker

workers :: [CronWorker ServerContext RequestContextM]
workers =
  [ persistentCommandRetryWorker
  ]

-- ------------------------------------------------------------------
persistentCommandRetryWorker :: CronWorker ServerContext RequestContextM
persistentCommandRetryWorker =
  CronWorker
    { name = "PersistentCommandRetryWorker"
    , condition = (.serverConfig.persistentCommand.retryJob.enabled)
    , cron = (.serverConfig.persistentCommand.retryJob.cron)
    , function = runPersistentCommands'
    , wrapInTransaction = True
    }
