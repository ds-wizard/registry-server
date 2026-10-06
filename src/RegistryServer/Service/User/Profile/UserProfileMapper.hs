module RegistryServer.Service.User.Profile.UserProfileMapper where

import Data.Time

import RegistryServer.Api.Resource.User.UserProfileChangeDTO
import RegistryServer.Model.User.User

fromUserProfileChangeDTO :: UserProfileChangeDTO -> User -> UTCTime -> User
fromUserProfileChangeDTO dto user now =
  user {firstName = dto.firstName, lastName = dto.lastName, email = dto.email, updatedAt = now}
