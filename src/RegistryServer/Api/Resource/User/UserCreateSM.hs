module RegistryServer.Api.Resource.User.UserCreateSM where

import Data.Swagger

import RegistryServer.Api.Resource.User.UserCreateDTO
import RegistryServer.Api.Resource.User.UserCreateJM ()
import RegistryServer.Database.Migration.Development.User.Data.Users
import Shared.Util.Swagger

instance ToSchema UserCreateDTO where
  declareNamedSchema = toSwagger userIsaacCreate
