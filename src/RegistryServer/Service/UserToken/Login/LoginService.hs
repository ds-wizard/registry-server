module RegistryServer.Service.UserToken.Login.LoginService where

import Control.Monad.Except (throwError)
import Control.Monad.Reader (liftIO)
import Data.Time

import RegistryServer.Api.Resource.UserToken.LoginDTO
import RegistryServer.Api.Resource.UserToken.UserTokenDTO
import RegistryServer.Constant.UserToken
import RegistryServer.Database.DAO.Common
import RegistryServer.Database.DAO.User.UserDAO
import RegistryServer.Localization.Messages.Public
import RegistryServer.Model.Context.RequestContext
import RegistryServer.Model.User.User
import RegistryServer.Model.UserToken.UserToken
import RegistryServer.Service.UserToken.Login.LoginValidation
import RegistryServer.Service.UserToken.UserTokenService
import Shared.Model.Error.Error

createLoginTokenFromCredentials :: LoginDTO -> RequestContextM UserTokenDTO
createLoginTokenFromCredentials reqDto =
  runInTransaction $ do
    mUser <- findUserByEmail' reqDto.email
    case mUser of
      Just user -> do
        validate reqDto user
        createLoginToken user
      Nothing -> throwError $ UserError _ERROR_SERVICE_TOKEN__INCORRECT_EMAIL_OR_PASSWORD

createLoginToken :: User -> RequestContextM UserTokenDTO
createLoginToken user = do
  now <- liftIO getCurrentTime
  createToken user.uuid "Login" LoginUserTokenType (Just $ addUTCTime loginTokenExpiration now)
