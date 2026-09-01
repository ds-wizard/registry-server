module RegistryServer.Model.Context.RequestContextHelpers where

import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks)

import RegistryPublic.Model.Organization.Organization
import RegistryPublic.Model.Organization.OrganizationRole
import RegistryServer.Localization.Messages.Public
import RegistryServer.Model.Context.RequestContext
import Shared.Model.Error.Error

getCurrentOrganization :: RequestContextM Organization
getCurrentOrganization = do
  mCurrentOrganization <- asks currentOrganization
  case mCurrentOrganization of
    Just org -> return org
    Nothing -> throwError $ ForbiddenError _ERROR_MODEL_APP_CONTEXT__MISSING_ORGANIZATION

isOrganizationAdmin :: RequestContextM Bool
isOrganizationAdmin = do
  mOrg <- asks currentOrganization
  case mOrg of
    Just org -> return $ org.oRole == AdminRole
    Nothing -> return False
