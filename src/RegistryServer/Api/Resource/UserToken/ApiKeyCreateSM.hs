module RegistryServer.Api.Resource.UserToken.ApiKeyCreateSM where

import Data.Swagger

import RegistryServer.Api.Resource.UserToken.ApiKeyCreateDTO
import RegistryServer.Api.Resource.UserToken.ApiKeyCreateJM ()
import RegistryServer.Database.Migration.Development.UserToken.Data.UserTokens
import Shared.Util.Swagger

instance ToSchema ApiKeyCreateDTO where
  declareNamedSchema = toSwagger apiKeyCreateDto
