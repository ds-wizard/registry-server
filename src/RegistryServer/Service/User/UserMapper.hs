module RegistryServer.Service.User.UserMapper where

import Data.Time
import qualified Data.UUID as U

import RegistryServer.Api.Resource.User.UserCreateDTO
import RegistryServer.Api.Resource.User.UserDTO
import RegistryServer.Model.User.User

toDTO :: User -> UserDTO
toDTO user =
  UserDTO
    { uuid = user.uuid
    , firstName = user.firstName
    , lastName = user.lastName
    , email = user.email
    , role = user.role
    , active = user.active
    , createdAt = user.createdAt
    , updatedAt = user.updatedAt
    }

fromUserCreateDTO :: UserCreateDTO -> U.UUID -> String -> UTCTime -> Bool -> User
fromUserCreateDTO dto uuid passwordHash now shouldSendRegistrationEmail =
  User
    { uuid = uuid
    , email = dto.email
    , firstName = dto.firstName
    , lastName = dto.lastName
    , passwordHash = passwordHash
    , role = UserRole
    , active = not shouldSendRegistrationEmail
    , createdAt = now
    , updatedAt = now
    }
