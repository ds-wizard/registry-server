module RegistryServer.Service.DocumentTemplate.Bundle.DocumentTemplateBundleAcl where

import Control.Monad.Except (throwError)

import RegistryPublic.Model.Organization.Organization
import RegistryPublic.Model.Organization.OrganizationRole
import RegistryServer.Model.Context.RequestContextHelpers
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error

checkWritePermission = do
  currentOrg <- getCurrentOrganization
  if currentOrg.oRole == AdminRole
    then return ()
    else throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Write DocumentTemplateBundle"
