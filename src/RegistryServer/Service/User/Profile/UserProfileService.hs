module RegistryServer.Service.User.Profile.UserProfileService where

import Control.Monad (void)
import Control.Monad.Reader (liftIO)
import Data.Time
import qualified Data.UUID as U

import RegistryServer.Api.Resource.User.UserDTO
import RegistryServer.Api.Resource.User.UserPasswordDTO
import RegistryServer.Api.Resource.User.UserProfileChangeDTO
import RegistryServer.Database.DAO.User.UserDAO
import RegistryServer.Model.Context.RequestContext
import RegistryServer.Model.Context.RequestContextHelpers
import RegistryServer.Model.User.User
import RegistryServer.Service.User.Profile.UserProfileMapper
import RegistryServer.Service.User.UserMapper
import RegistryServer.Service.User.UserValidation
import Shared.Util.Password

getUserProfile :: RequestContextM UserDTO
getUserProfile = toDTO <$> getCurrentUser

modifyUserProfile :: UserProfileChangeDTO -> RequestContextM UserDTO
modifyUserProfile reqDto = do
  user <- getCurrentUser
  validateUserChangedEmailUniqueness reqDto.email user.email
  now <- liftIO getCurrentTime
  let updatedUser = fromUserProfileChangeDTO reqDto user now
  updateUserByUuid updatedUser
  return . toDTO $ updatedUser

changeUserProfilePassword :: U.UUID -> UserPasswordDTO -> RequestContextM ()
changeUserProfilePassword userUuid reqDto = do
  passwordHash <- generatePasswordHash reqDto.password
  now <- liftIO getCurrentTime
  void $ updateUserPasswordByUuid userUuid passwordHash now
