module RegistryServer.Service.Organization.OrganizationService where

import Control.Monad (when)
import Control.Monad.Except (catchError, throwError)
import Control.Monad.Reader (asks, liftIO)
import Data.Time

import RegistryPublic.Api.Resource.Organization.OrganizationCreateDTO
import RegistryPublic.Api.Resource.Organization.OrganizationDTO
import RegistryPublic.Api.Resource.Organization.OrganizationStateDTO
import RegistryPublic.Model.Organization.Organization
import RegistryPublic.Model.Organization.OrganizationRole
import RegistryPublic.Model.Organization.OrganizationSimple
import RegistryServer.Api.Resource.Organization.OrganizationChangeDTO
import RegistryServer.Database.DAO.Common
import RegistryServer.Database.DAO.Organization.OrganizationDAO
import RegistryServer.Localization.Messages.Internal
import RegistryServer.Model.Config.ServerConfig
import RegistryServer.Model.Context.RequestContext
import RegistryServer.Model.Context.RequestContextHelpers
import RegistryServer.Model.UserEmailLink.UserEmailLinkType
import RegistryServer.Service.Mail.Mailer
import RegistryServer.Service.Organization.OrganizationMapper
import RegistryServer.Service.Organization.OrganizationValidation
import RegistryServer.Service.UserEmailLink.UserEmailLinkService
import Shared.Api.Resource.UserEmailLink.UserEmailLinkDTO
import Shared.Database.DAO.UserEmailLink.UserEmailLinkDAO
import Shared.Localization.Messages.Public
import Shared.Model.Config.ServerConfig
import Shared.Model.Error.Error
import Shared.Model.UserEmailLink.UserEmailLink
import Shared.Util.Crypto (generateRandomString)

getOrganizations :: RequestContextM [OrganizationDTO]
getOrganizations = do
  _ <- checkPermissionToListOrganizations
  organizations <- findOrganizations
  return . fmap toDTO $ organizations

getSimpleOrganizations :: RequestContextM [OrganizationSimple]
getSimpleOrganizations = findUsedOrganizations

createOrganization :: OrganizationCreateDTO -> Maybe String -> RequestContextM OrganizationDTO
createOrganization reqDto mCallbackUrl =
  runInTransaction $ do
    _ <- validateOrganizationCreateDto reqDto
    token <- generateNewOrgToken
    now <- liftIO getCurrentTime
    let org = fromCreateDTO reqDto UserRole token now now now
    insertOrganization org
    userEmailLink <- createUserEmailLink org.organizationId RegistrationUserEmailLinkType
    _ <-
      sendRegistrationConfirmationMail (toDTO org) userEmailLink.hash mCallbackUrl
        `catchError` (\errMessage -> throwError $ GeneralServerError _ERROR_SERVICE_ORGANIZATION__ACTIVATION_EMAIL_NOT_SENT)
    sendAnalyticsEmailIfEnabled org
    return . toDTO $ org
  where
    sendAnalyticsEmailIfEnabled org = do
      serverConfig <- asks serverConfig
      when serverConfig.analyticalMails.enabled $ sendRegistrationCreatedAnalyticsMail (toDTO org)

getOrganizationByOrgId :: String -> RequestContextM OrganizationDTO
getOrganizationByOrgId orgId = do
  organization <- findOrganizationByOrgId orgId
  _ <- checkPermissionToOrganization organization
  return . toDTO $ organization

getOrganizationByToken :: String -> RequestContextM OrganizationDTO
getOrganizationByToken token = do
  organization <- findOrganizationByToken token
  _ <- checkPermissionToOrganization organization
  return . toDTO $ organization

modifyOrganization :: String -> OrganizationChangeDTO -> RequestContextM OrganizationDTO
modifyOrganization orgId reqDto =
  runInTransaction $ do
    org <- getOrganizationByOrgId orgId
    _ <- validateOrganizationChangedEmailUniqueness reqDto.email org.email
    now <- liftIO getCurrentTime
    let organization = fromChangeDTO reqDto org now
    updateOrganization organization
    return . toDTO $ organization

deleteOrganization :: String -> RequestContextM (Maybe AppError)
deleteOrganization orgId =
  runInTransaction $ do
    org <- getOrganizationByOrgId orgId
    deleteOrganizationByOrgId orgId
    return Nothing

changeOrganizationTokenByHash :: String -> String -> RequestContextM OrganizationDTO
changeOrganizationTokenByHash orgId hash =
  runInTransaction $ do
    userEmailLink <- findUserEmailLinkByHash hash :: RequestContextM (UserEmailLink String UserEmailLinkType)
    org <- findOrganizationByOrgId userEmailLink.identity
    orgToken <- generateNewOrgToken
    now <- liftIO getCurrentTime
    let updatedOrg = org {token = orgToken, updatedAt = now} :: Organization
    updateOrganization updatedOrg
    deleteUserEmailLinkByHash userEmailLink.hash
    return . toDTO $ updatedOrg

resetOrganizationToken :: UserEmailLinkDTO UserEmailLinkType -> RequestContextM ()
resetOrganizationToken reqDto =
  runInTransaction $ do
    validateOrganizationEmailExistence reqDto.email
    org <- findOrganizationByEmail reqDto.email
    userEmailLink <- createUserEmailLink org.organizationId ForgottenTokenUserEmailLinkType
    _ <-
      sendResetTokenMail (toDTO org) userEmailLink.hash
        `catchError` (\errMessage -> throwError $ GeneralServerError _ERROR_SERVICE_ORGANIZATION__RECOVERY_EMAIL_NOT_SENT)
    return ()

changeOrganizationState :: String -> String -> OrganizationStateDTO -> RequestContextM OrganizationDTO
changeOrganizationState orgId hash reqDto =
  runInTransaction $ do
    userEmailLink <- findUserEmailLinkByHash hash :: RequestContextM (UserEmailLink String UserEmailLinkType)
    org <- findOrganizationByOrgId userEmailLink.identity
    updatedOrg <- updateOrgTimestamp $ org {active = reqDto.active}
    updateOrganization updatedOrg
    deleteUserEmailLinkByHash userEmailLink.hash
    return . toDTO $ updatedOrg

-- --------------------------------
-- PERMISSIONS
-- --------------------------------
checkPermissionToListOrganizations = do
  currentOrg <- getCurrentOrganization
  if currentOrg.oRole == AdminRole
    then return ()
    else throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "List Organizations"

checkPermissionToOrganization org = do
  currentOrg <- getCurrentOrganization
  if currentOrg.oRole == AdminRole || org.organizationId == currentOrg.organizationId
    then return ()
    else throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Detail Organization"

-- --------------------------------
-- PRIVATE
-- --------------------------------
generateNewOrgToken :: RequestContextM String
generateNewOrgToken = liftIO $ generateRandomString 256

updateOrgTimestamp :: Organization -> RequestContextM Organization
updateOrgTimestamp org = do
  now <- liftIO getCurrentTime
  return $ org {updatedAt = now}
