module RegistryServer.Api.Resource.UserToken.UserTokenListSM where

import Data.Swagger

import RegistryServer.Api.Resource.UserToken.UserTokenListJM ()
import RegistryServer.Database.Migration.Development.UserToken.Data.UserTokens
import RegistryServer.Model.UserToken.UserTokenList
import Shared.Util.Swagger

instance ToSchema UserTokenList where
  declareNamedSchema = toSwagger adminApiKeyList
