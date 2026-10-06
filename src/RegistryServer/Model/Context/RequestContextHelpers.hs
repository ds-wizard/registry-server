module RegistryServer.Model.Context.RequestContextHelpers where

import Control.Monad (unless)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks)

import RegistryServer.Localization.Messages.Public
import RegistryServer.Model.Context.RequestContext
import RegistryServer.Model.User.User
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error

getCurrentUser :: RequestContextM User
getCurrentUser = do
  mCurrentUser <- asks (.currentUser)
  case mCurrentUser of
    Just user -> return user
    Nothing -> throwError $ ForbiddenError _ERROR_MODEL_APP_CONTEXT__MISSING_USER

isAdmin :: RequestContextM Bool
isAdmin = do
  mUser <- asks (.currentUser)
  case mUser of
    Just user -> return $ user.role == AdminRole
    Nothing -> return False

checkAdminRole :: String -> RequestContextM ()
checkAdminRole action = do
  user <- getCurrentUser
  unless (user.role == AdminRole) (throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN action)
