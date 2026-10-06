module RegistryServer.Service.PersistentCommand.PersistentCommandExecutor where

import Control.Monad.Except (throwError)

import RegistryServer.Model.Context.RequestContext
import Shared.Model.Error.Error
import Shared.Model.PersistentCommand.PersistentCommand

components :: [String]
components = []

execute :: PersistentCommand String -> RequestContextM (PersistentCommandState, Maybe String)
execute command = throwError . GeneralServerError $ "Unknown command component: " <> command.component
