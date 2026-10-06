module RegistryServer.Service.UserToken.Login.LoginValidation where

import Control.Monad.Except (throwError)

import RegistryServer.Api.Resource.UserToken.LoginDTO
import RegistryServer.Localization.Messages.Public
import RegistryServer.Model.Context.RequestContext
import RegistryServer.Model.User.User
import Shared.Model.Error.Error
import Shared.Util.Password

validate :: LoginDTO -> User -> RequestContextM ()
validate reqDto user = do
  validateUserPassword reqDto user
  validateIsUserActive user

validateIsUserActive :: User -> RequestContextM ()
validateIsUserActive user =
  if user.active
    then return ()
    else throwError $ UserError _ERROR_SERVICE_TOKEN__ACCOUNT_IS_NOT_ACTIVATED

validateUserPassword :: LoginDTO -> User -> RequestContextM ()
validateUserPassword reqDto user =
  if verifyPassword reqDto.password user.passwordHash
    then return ()
    else throwError $ UserError _ERROR_SERVICE_TOKEN__INCORRECT_EMAIL_OR_PASSWORD
