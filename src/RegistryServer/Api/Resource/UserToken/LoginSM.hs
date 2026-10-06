module RegistryServer.Api.Resource.UserToken.LoginSM where

import Data.Swagger

import RegistryServer.Api.Resource.UserToken.LoginDTO
import RegistryServer.Api.Resource.UserToken.LoginJM ()
import RegistryServer.Database.Migration.Development.UserToken.Data.UserTokens
import Shared.Util.Swagger

instance ToSchema LoginDTO where
  declareNamedSchema = toSwagger adminLoginDto
