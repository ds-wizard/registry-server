module RegistryServer.Api.Resource.PersistentCommand.PersistentCommandSM where

import Data.Swagger

import RegistryServer.Database.Migration.Development.PersistentCommand.Data.PersistentCommands
import Shared.Api.Resource.PersistentCommand.PersistentCommandSM ()
import Shared.Model.PersistentCommand.PersistentCommand
import Shared.Util.Swagger

instance ToSchema (PersistentCommand String) where
  declareNamedSchema = toSwagger command1
