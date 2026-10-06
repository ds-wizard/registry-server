module RegistryServer.Service.PersistentCommand.PersistentCommandService where

import RegistryServer.Database.DAO.Common
import RegistryServer.Model.Context.ContextMappers
import RegistryServer.Model.Context.RequestContext
import RegistryServer.Model.Context.RequestContextHelpers
import RegistryServer.Service.PersistentCommand.PersistentCommandExecutor
import Shared.Database.DAO.PersistentCommand.PersistentCommandDAO
import Shared.Model.PersistentCommand.PersistentCommand
import Shared.Model.PersistentCommand.PersistentCommandSimple
import Shared.Service.PersistentCommand.PersistentCommandService

createPersistentCommand :: PersistentCommand String -> RequestContextM (PersistentCommand String)
createPersistentCommand persistentCommand =
  runInTransaction $ do
    checkAdminRole "Create Persistent Command"
    mPersistentCommandFromDb <- findPersistentCommandByUuid' persistentCommand.uuid :: RequestContextM (Maybe (PersistentCommand String))
    case mPersistentCommandFromDb of
      Just _ -> return persistentCommand
      Nothing -> do
        insertPersistentCommand persistentCommand
        return persistentCommand

runPersistentCommands' :: RequestContextM ()
runPersistentCommands' = runPersistentCommands runRequestContextWithRequestContext' updateContext execute components

runPersistentCommandChannelListener' :: RequestContextM ()
runPersistentCommandChannelListener' = runPersistentCommandChannelListener runRequestContextWithRequestContext' updateContext execute components

updateContext :: PersistentCommandSimple String -> RequestContext -> RequestContextM RequestContext
updateContext commandSimple = return
