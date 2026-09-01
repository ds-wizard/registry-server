module RegistryServer.Service.Config.Server.ServerConfigValidation where

import RegistryServer.Model.Config.ServerConfig
import Shared.Model.Error.Error
import Shared.Service.Config.Server.ServerConfigValidation

validateServerConfig :: ServerConfig -> Either AppError ServerConfig
validateServerConfig config = do
  validateGeneralServerPort config
  validateDatabaseMaxConnections config
