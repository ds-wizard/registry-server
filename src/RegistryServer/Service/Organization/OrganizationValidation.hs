module RegistryServer.Service.Organization.OrganizationValidation where

import Control.Monad (unless, when)
import Control.Monad.Except (throwError)
import qualified Data.Map.Strict as M

import RegistryPublic.Api.Resource.Organization.OrganizationCreateDTO
import RegistryServer.Database.DAO.Organization.OrganizationDAO
import RegistryServer.Localization.Messages.Public
import RegistryServer.Model.Config.ServerConfig
import RegistryServer.Model.Context.RequestContext
import RegistryServer.Model.Context.RequestContextHelpers
import RegistryServer.Service.Common
import Shared.Model.Error.Error
import Shared.Service.Coordinate.CoordinateValidation

validateOrganizationCreateDto :: OrganizationCreateDTO -> RequestContextM ()
validateOrganizationCreateDto reqDto = do
  validatePublicRegistrationEnabled
  _ <- validateOrganizationIdUniqueness reqDto.organizationId
  _ <- validateOrganizationEmailUniqueness reqDto.email
  validateCoordinatePartFormat "organizationId" reqDto.organizationId

validatePublicRegistrationEnabled :: RequestContextM ()
validatePublicRegistrationEnabled = do
  isAdmin <- isOrganizationAdmin
  unless
    isAdmin
    (checkIfServerFeatureIsEnabled "Tenant Registration" (\s -> s.general.publicRegistrationEnabled))

validateOrganizationIdUniqueness :: String -> RequestContextM ()
validateOrganizationIdUniqueness orgId = do
  mOrg <- findOrganizationByOrgId' orgId
  case mOrg of
    Just _ ->
      throwError $
        ValidationError [] (M.singleton "organizationId" [_ERROR_VALIDATION__ORGANIZATION_ID_UNIQUENESS orgId])
    Nothing -> return ()

validateOrganizationEmailUniqueness :: String -> RequestContextM ()
validateOrganizationEmailUniqueness email = do
  mOrg <- findOrganizationByEmail' email
  case mOrg of
    Just _ ->
      throwError $ ValidationError [] (M.singleton "email" [_ERROR_VALIDATION__ORGANIZATION_EMAIL_UNIQUENESS email])
    Nothing -> return ()

validateOrganizationEmailExistence :: String -> RequestContextM ()
validateOrganizationEmailExistence email = do
  mOrg <- findOrganizationByEmail' email
  case mOrg of
    Just _ -> return ()
    Nothing -> throwError $ UserError (_ERROR_VALIDATION__ORGANIZATION_EMAIL_ABSENCE email)

validateOrganizationChangedEmailUniqueness :: String -> String -> RequestContextM ()
validateOrganizationChangedEmailUniqueness newEmail oldEmail =
  when (newEmail /= oldEmail) $ validateOrganizationEmailUniqueness newEmail
