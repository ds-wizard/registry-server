module RegistryServer.Api.Resource.User.UserProfileChangeSM where

import Data.Swagger

import RegistryServer.Api.Resource.User.UserProfileChangeDTO
import RegistryServer.Api.Resource.User.UserProfileChangeJM ()
import RegistryServer.Database.Migration.Development.User.Data.Users
import Shared.Util.Swagger

instance ToSchema UserProfileChangeDTO where
  declareNamedSchema = toSwagger userAdminProfileChange
