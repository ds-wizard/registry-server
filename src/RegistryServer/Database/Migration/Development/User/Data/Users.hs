module RegistryServer.Database.Migration.Development.User.Data.Users where

import Data.Maybe (fromJust)
import Data.Time
import qualified Data.UUID as U

import RegistryServer.Api.Resource.User.UserCreateDTO
import RegistryServer.Api.Resource.User.UserDTO
import RegistryServer.Api.Resource.User.UserPasswordDTO
import RegistryServer.Api.Resource.User.UserProfileChangeDTO
import RegistryServer.Api.Resource.User.UserStateDTO
import RegistryServer.Model.User.User
import RegistryServer.Service.User.UserMapper

userAdmin :: User
userAdmin =
  User
    { uuid = fromJust . U.fromString $ "4e2a6c9f-3d7b-4b8e-9c1a-5f0d2e8b7a61"
    , email = "albert.einstein@example.com"
    , firstName = "Albert"
    , lastName = "Einstein"
    , -- cspell:disable
      passwordHash = "pbkdf1:sha256|17|awVwfF3h27PrxINtavVgFQ==|iUFbQnZFv+rBXBu1R2OkX+vEjPtohYk5lsyIeOBdEy4="
    , -- cspell:enable
      role = AdminRole
    , active = True
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

userAdminDTO :: UserDTO
userAdminDTO = toDTO userAdmin

userAdminProfileChange :: UserProfileChangeDTO
userAdminProfileChange =
  UserProfileChangeDTO
    { email = "edited.albert.einstein@example.com"
    , firstName = "EDITED: Albert"
    , lastName = "EDITED: Einstein"
    }

userNikola :: User
userNikola =
  User
    { uuid = fromJust . U.fromString $ "7b1f0d3e-8a2c-4f6b-b5d9-2c4e6a8f0b13"
    , email = "nikola.tesla@example.com"
    , firstName = "Nikola"
    , lastName = "Tesla"
    , -- cspell:disable
      passwordHash = "pbkdf1:sha256|17|awVwfF3h27PrxINtavVgFQ==|iUFbQnZFv+rBXBu1R2OkX+vEjPtohYk5lsyIeOBdEy4="
    , -- cspell:enable
      role = UserRole
    , active = True
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

userNikolaDTO :: UserDTO
userNikolaDTO = toDTO userNikola

userIsaacCreate :: UserCreateDTO
userIsaacCreate =
  UserCreateDTO
    { email = "isaac.newton@example.com"
    , firstName = "Isaac"
    , lastName = "Newton"
    , password = "password"
    }

userStateDto :: UserStateDTO
userStateDto = UserStateDTO {active = True}

userPasswordDto :: UserPasswordDTO
userPasswordDto = UserPasswordDTO {password = "newPassword"}
