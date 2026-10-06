module RegistryServer.Api.Resource.User.UserStateSM where

import Data.Swagger

import RegistryServer.Api.Resource.User.UserStateDTO
import RegistryServer.Api.Resource.User.UserStateJM ()
import RegistryServer.Database.Migration.Development.User.Data.Users
import Shared.Util.Swagger

instance ToSchema UserStateDTO where
  declareNamedSchema = toSwagger userStateDto
