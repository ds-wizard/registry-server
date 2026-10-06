module RegistryServer.Api.Resource.UserToken.UserTokenSM where

import Data.Swagger

import RegistryServer.Api.Resource.UserToken.UserTokenDTO
import RegistryServer.Api.Resource.UserToken.UserTokenJM ()
import RegistryServer.Database.Migration.Development.UserToken.Data.UserTokens
import Shared.Util.Swagger

instance ToSchema UserTokenDTO where
  declareNamedSchema = toSwaggerWithType "type" adminUserTokenDto
