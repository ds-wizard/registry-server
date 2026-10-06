module RegistryServer.Api.Resource.User.UserRoleSM where

import Data.Swagger

import RegistryServer.Api.Resource.User.UserRoleJM ()
import RegistryServer.Model.User.User

instance ToSchema UserRole
