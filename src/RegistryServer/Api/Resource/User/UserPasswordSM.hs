module RegistryServer.Api.Resource.User.UserPasswordSM where

import Data.Swagger

import RegistryServer.Api.Resource.User.UserPasswordDTO
import RegistryServer.Api.Resource.User.UserPasswordJM ()
import RegistryServer.Database.Migration.Development.User.Data.Users
import Shared.Util.Swagger

instance ToSchema UserPasswordDTO where
  declareNamedSchema = toSwagger userPasswordDto
