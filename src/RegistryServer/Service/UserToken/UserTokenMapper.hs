module RegistryServer.Service.UserToken.UserTokenMapper where

import Data.Time
import qualified Data.UUID as U

import RegistryServer.Api.Resource.UserToken.UserTokenDTO
import RegistryServer.Model.UserToken.UserToken
import RegistryServer.Model.UserToken.UserTokenList
import Shared.Util.Crypto (hashSHA256)

toDTO :: String -> UserToken -> UserTokenDTO
toDTO value token = UserTokenDTO {token = value, expiresAt = token.expiresAt}

toList :: UserToken -> UserTokenList
toList token =
  UserTokenList
    { uuid = token.uuid
    , name = token.name
    , expiresAt = token.expiresAt
    , createdAt = token.createdAt
    }

toUserToken :: U.UUID -> String -> UserTokenType -> U.UUID -> String -> Maybe UTCTime -> UTCTime -> UserToken
toUserToken uuid name tokenType userUuid value expiresAt now =
  UserToken
    { uuid = uuid
    , name = name
    , tType = tokenType
    , userUuid = userUuid
    , valueHash = hashSHA256 value
    , expiresAt = expiresAt
    , createdAt = now
    }
