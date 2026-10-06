module RegistryServer.Database.Migration.Development.UserToken.Data.UserTokens where

import Data.Maybe (fromJust)
import Data.Time
import qualified Data.UUID as U

import RegistryServer.Api.Resource.UserToken.ApiKeyCreateDTO
import RegistryServer.Api.Resource.UserToken.LoginDTO
import RegistryServer.Api.Resource.UserToken.UserTokenDTO
import RegistryServer.Database.Migration.Development.User.Data.Users
import RegistryServer.Model.User.User
import RegistryServer.Model.UserToken.UserToken
import RegistryServer.Model.UserToken.UserTokenList
import RegistryServer.Service.UserToken.UserTokenMapper
import Shared.Util.Crypto (hashSHA256)

adminApiKey :: UserToken
adminApiKey =
  UserToken
    { uuid = fromJust . U.fromString $ "0c5e8f2a-6b4d-4e1f-9a3c-7d2b5e8f1a04"
    , name = "Global API key"
    , tType = ApiKeyUserTokenType
    , userUuid = userAdmin.uuid
    , valueHash = hashSHA256 "GlobalToken"
    , expiresAt = Nothing
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    }

adminApiKeyList :: UserTokenList
adminApiKeyList = toList adminApiKey

nikolaApiKey :: UserToken
nikolaApiKey =
  UserToken
    { uuid = fromJust . U.fromString $ "9d3b1e7c-2f5a-4c8d-b6e0-1a4f7c9e2b35"
    , name = "Netherlands API key"
    , tType = ApiKeyUserTokenType
    , userUuid = userNikola.uuid
    , valueHash = hashSHA256 "NetherlandsToken"
    , expiresAt = Nothing
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    }

nikolaExpiredLoginToken :: UserToken
nikolaExpiredLoginToken =
  UserToken
    { uuid = fromJust . U.fromString $ "3f8a2c6e-7d1b-4a9f-8e5c-2b6d0f4a7c18"
    , name = "Login"
    , tType = LoginUserTokenType
    , userUuid = userNikola.uuid
    , valueHash = hashSHA256 "ExpiredToken"
    , expiresAt = Just $ UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    }

adminLoginDto :: LoginDTO
adminLoginDto = LoginDTO {email = userAdmin.email, password = "password"}

adminUserTokenDto :: UserTokenDTO
adminUserTokenDto = UserTokenDTO {token = "GlobalToken", expiresAt = Nothing}

apiKeyCreateDto :: ApiKeyCreateDTO
apiKeyCreateDto = ApiKeyCreateDTO {name = "My API key"}
