module RegistryServer.Service.User.UserService where

import Control.Monad (void, when)
import Control.Monad.Except (catchError, throwError)
import Control.Monad.Reader (asks, liftIO)
import Data.Maybe (fromMaybe)
import Data.Time
import qualified Data.UUID as U

import RegistryServer.Api.Resource.User.UserCreateDTO
import RegistryServer.Api.Resource.User.UserDTO
import RegistryServer.Api.Resource.User.UserPasswordDTO
import RegistryServer.Database.DAO.Common
import RegistryServer.Database.DAO.User.UserDAO
import RegistryServer.Localization.Messages.Internal
import RegistryServer.Model.Config.ServerConfig
import RegistryServer.Model.Context.RequestContext
import RegistryServer.Model.Context.RequestContextHelpers
import RegistryServer.Model.User.User
import RegistryServer.Model.UserEmailLink.UserEmailLinkType
import RegistryServer.Service.Common
import RegistryServer.Service.Mail.Mailer
import RegistryServer.Service.User.UserMapper
import RegistryServer.Service.User.UserValidation
import RegistryServer.Service.UserEmailLink.UserEmailLinkService
import Shared.Api.Resource.UserEmailLink.UserEmailLinkDTO
import Shared.Database.DAO.UserEmailLink.UserEmailLinkDAO
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Shared.Model.Config.ServerConfig
import Shared.Model.Error.Error
import Shared.Model.UserEmailLink.UserEmailLink
import Shared.Util.Password
import Shared.Util.Uuid

getUsersPage :: Maybe String -> Maybe String -> Pageable -> [Sort] -> RequestContextM (Page UserDTO)
getUsersPage mQuery mRole pageable sort = do
  checkAdminRole "List Users"
  userPage <- findUsersPage mQuery mRole pageable sort
  return . fmap toDTO $ userPage

registerOrCreateUserByAdmin :: UserCreateDTO -> RequestContextM UserDTO
registerOrCreateUserByAdmin reqDto =
  runInTransaction $ do
    admin <- isAdmin
    if admin
      then createUserByAdmin reqDto
      else registerUser reqDto

createUserByAdmin :: UserCreateDTO -> RequestContextM UserDTO
createUserByAdmin reqDto =
  runInTransaction $ do
    checkAdminRole "Create User"
    uUuid <- liftIO generateUuid
    uPasswordHash <- generatePasswordHash reqDto.password
    createUser reqDto uUuid uPasswordHash False

registerUser :: UserCreateDTO -> RequestContextM UserDTO
registerUser reqDto =
  runInTransaction $ do
    checkIfRegistrationIsEnabled
    uUuid <- liftIO generateUuid
    uPasswordHash <- generatePasswordHash reqDto.password
    mExistingUser <- findUserByEmail' reqDto.email
    case mExistingUser of
      Just _ -> do
        now <- liftIO getCurrentTime
        return . toDTO $ fromUserCreateDTO reqDto uUuid uPasswordHash now True
      Nothing -> createUser reqDto uUuid uPasswordHash True

createUser :: UserCreateDTO -> U.UUID -> String -> Bool -> RequestContextM UserDTO
createUser reqDto uUuid uPasswordHash shouldSendRegistrationEmail =
  runInTransaction $ do
    validateUserEmailUniqueness reqDto.email
    now <- liftIO getCurrentTime
    let user = fromUserCreateDTO reqDto uUuid uPasswordHash now shouldSendRegistrationEmail
    insertUser user
    userEmailLink <- createUserEmailLink (U.toString uUuid) RegistrationUserEmailLinkType
    when
      shouldSendRegistrationEmail
      ( catchError
          (sendRegistrationConfirmationMail user userEmailLink.hash)
          (\_ -> throwError $ GeneralServerError _ERROR_SERVICE_USER__ACTIVATION_EMAIL_NOT_SENT)
      )
    sendAnalyticsEmailIfEnabled user
    return $ toDTO user

getUserDetailById :: U.UUID -> RequestContextM UserDTO
getUserDetailById userUuid = do
  checkAdminRole "Detail User"
  toDTO <$> findUserByUuid userUuid

changeUserPasswordByAdminOrHash :: U.UUID -> UserPasswordDTO -> Maybe String -> RequestContextM ()
changeUserPasswordByAdminOrHash userUuid reqDto mHash =
  runInTransaction $ do
    admin <- isAdmin
    if admin
      then changeUserPasswordByAdmin userUuid reqDto
      else do
        let hash = fromMaybe (U.toString U.nil) mHash
        changeUserPasswordByHash hash reqDto

changeUserPasswordByAdmin :: U.UUID -> UserPasswordDTO -> RequestContextM ()
changeUserPasswordByAdmin userUuid reqDto =
  runInTransaction $ do
    user <- findUserByUuid userUuid
    updateUserPassword user reqDto

changeUserPasswordByHash :: String -> UserPasswordDTO -> RequestContextM ()
changeUserPasswordByHash hash reqDto =
  runInTransaction $ do
    (user, userEmailLink) <- findUserByHash hash ForgottenPasswordUserEmailLinkType
    updateUserPassword user reqDto
    void $ deleteUserEmailLinkByHash userEmailLink.hash

resetUserPassword :: UserEmailLinkDTO UserEmailLinkType -> RequestContextM ()
resetUserPassword reqDto =
  runInTransaction $ do
    mUser <- findUserByEmail' reqDto.email
    case mUser of
      Just user -> do
        userEmailLink <- createUserEmailLink (U.toString user.uuid) ForgottenPasswordUserEmailLinkType
        catchError
          (sendResetPasswordMail user userEmailLink.hash)
          (\_ -> throwError $ GeneralServerError _ERROR_SERVICE_USER__RECOVERY_EMAIL_NOT_SENT)
      Nothing -> return ()

changeUserState :: String -> Bool -> RequestContextM ()
changeUserState hash active =
  runInTransaction $ do
    (user, userEmailLink) <- findUserByHash hash RegistrationUserEmailLinkType
    now <- liftIO getCurrentTime
    let updatedUser = user {active = active, updatedAt = now} :: User
    updateUserByUuid updatedUser
    void $ deleteUserEmailLinkByHash userEmailLink.hash

deleteUser :: U.UUID -> RequestContextM ()
deleteUser userUuid =
  runInTransaction $ do
    checkAdminRole "Delete User"
    _ <- findUserByUuid userUuid
    deleteUserEmailLinkByIdentity (U.toString userUuid)
    void $ deleteUserByUuid userUuid

-- --------------------------------
-- PRIVATE
-- --------------------------------
sendAnalyticsEmailIfEnabled :: User -> RequestContextM ()
sendAnalyticsEmailIfEnabled user = do
  serverConfig <- asks (.serverConfig)
  when serverConfig.analyticalMails.enabled (sendRegistrationCreatedAnalyticsMail user)

checkIfRegistrationIsEnabled :: RequestContextM ()
checkIfRegistrationIsEnabled = checkIfServerFeatureIsEnabled "Registration" (\s -> s.general.publicRegistrationEnabled)

findUserByHash :: String -> UserEmailLinkType -> RequestContextM (User, UserEmailLink String UserEmailLinkType)
findUserByHash hash linkType = do
  userEmailLink <- findUserEmailLinkByHashAndType hash linkType
  user <- findUserByUuid (u' userEmailLink.identity)
  return (user, userEmailLink)

updateUserPassword :: User -> UserPasswordDTO -> RequestContextM ()
updateUserPassword user reqDto = do
  passwordHash <- generatePasswordHash reqDto.password
  now <- liftIO getCurrentTime
  void $ updateUserPasswordByUuid user.uuid passwordHash now
