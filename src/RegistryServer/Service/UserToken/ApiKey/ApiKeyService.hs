module RegistryServer.Service.UserToken.ApiKey.ApiKeyService where

import RegistryServer.Api.Resource.UserToken.ApiKeyCreateDTO
import RegistryServer.Api.Resource.UserToken.UserTokenDTO
import RegistryServer.Database.DAO.Common
import RegistryServer.Model.Context.RequestContext
import RegistryServer.Model.Context.RequestContextHelpers
import RegistryServer.Model.User.User
import RegistryServer.Model.UserToken.UserToken
import RegistryServer.Service.UserToken.UserTokenService

createApiKey :: ApiKeyCreateDTO -> RequestContextM UserTokenDTO
createApiKey reqDto =
  runInTransaction $ do
    user <- getCurrentUser
    createToken user.uuid reqDto.name ApiKeyUserTokenType Nothing
