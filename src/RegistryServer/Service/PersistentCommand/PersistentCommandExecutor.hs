module RegistryServer.Service.PersistentCommand.PersistentCommandExecutor where

import Control.Monad.Except (throwError)

import RegistryServer.Model.Context.RequestContext
import qualified RegistryServer.Service.Organization.OrganizationCommandExecutor as OrganizationCommandExecutor
import Shared.Model.Error.Error
import Shared.Model.PersistentCommand.PersistentCommand

components :: [String]
components =
  [ OrganizationCommandExecutor.cComponent
  ]

execute :: PersistentCommand String -> RequestContextM (PersistentCommandState, Maybe String)
execute command
  | command.component == OrganizationCommandExecutor.cComponent = OrganizationCommandExecutor.execute command
  | otherwise = throwError . GeneralServerError $ "Unknown command component: " <> command.component
