module RegistryServer.Service.User.UserValidation where

import Control.Monad (when)
import Control.Monad.Except (throwError)
import qualified Data.Map.Strict as M

import RegistryServer.Database.DAO.User.UserDAO
import RegistryServer.Localization.Messages.Public
import RegistryServer.Model.Context.RequestContext
import Shared.Model.Error.Error

validateUserEmailUniqueness :: String -> RequestContextM ()
validateUserEmailUniqueness email = do
  mUser <- findUserByEmail' email
  case mUser of
    Just _ -> throwError $ ValidationError [] (M.singleton "email" [_ERROR_VALIDATION__USER_EMAIL_UNIQUENESS email])
    Nothing -> return ()

validateUserChangedEmailUniqueness :: String -> String -> RequestContextM ()
validateUserChangedEmailUniqueness newEmail oldEmail =
  when (newEmail /= oldEmail) $ validateUserEmailUniqueness newEmail
