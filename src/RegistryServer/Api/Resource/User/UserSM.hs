module RegistryServer.Api.Resource.User.UserSM where

import Data.Swagger

import RegistryServer.Api.Resource.User.UserDTO
import RegistryServer.Api.Resource.User.UserJM ()
import RegistryServer.Api.Resource.User.UserRoleSM ()
import RegistryServer.Database.Migration.Development.User.Data.Users
import Shared.Util.Swagger

instance ToSchema UserDTO where
  declareNamedSchema = toSwagger userAdminDTO
