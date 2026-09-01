module RegistryServer.Database.Migration.Development.Organization.OrganizationMigration where

import RegistryPublic.Database.Migration.Development.Organization.Data.Organizations
import RegistryServer.Database.DAO.Organization.OrganizationDAO
import RegistryServer.Model.Context.ContextLenses ()
import RegistryServer.Model.Context.RequestContext
import Shared.Util.Logger

runMigration :: RequestContextM ()
runMigration = do
  logInfo _CMP_MIGRATION "(Fixtures/Organization) started"
  insertOrganization orgGlobal
  insertOrganization orgNetherlands
  logInfo _CMP_MIGRATION "(Fixtures/Organization) ended"
