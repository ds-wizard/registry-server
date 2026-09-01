module RegistryServer.Database.DAO.Organization.OrganizationDAO where

import Data.String
import qualified Data.Text as T
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import RegistryPublic.Model.Organization.Organization
import RegistryPublic.Model.Organization.OrganizationSimple
import RegistryServer.Database.DAO.Common
import RegistryServer.Database.Mapping.Organization.Organization ()
import RegistryServer.Database.Mapping.Organization.OrganizationSimple ()
import RegistryServer.Model.Context.ContextLenses ()
import RegistryServer.Model.Context.RequestContext
import Shared.Util.Logger
import Shared.Util.String

entityName = "organization"

findOrganizations :: RequestContextM [Organization]
findOrganizations = createFindEntitiesFn entityName

findUsedOrganizations :: RequestContextM [OrganizationSimple]
findUsedOrganizations = do
  let sql =
        "SELECT organization_id, name, logo \
        \FROM organization \
        \WHERE organization_id IN (SELECT DISTINCT nested.organization_id \
        \                          FROM (SELECT DISTINCT organization_id FROM knowledge_model_package \
        \                                UNION ALL \
        \                                SELECT DISTINCT organization_id FROM document_template \
        \                                UNION ALL \
        \                                SELECT DISTINCT organization_id FROM locale \
        \                         ) nested)"
  logInfoI _CMP_DATABASE (trim sql)
  let action conn = query_ conn (fromString sql)
  runDB action

findOrganizationByOrgId :: String -> RequestContextM Organization
findOrganizationByOrgId organizationId = createFindEntityByFn entityName [("organization_id", organizationId)]

findOrganizationByOrgId' :: String -> RequestContextM (Maybe Organization)
findOrganizationByOrgId' organizationId = createFindEntityByFn' entityName [("organization_id", organizationId)]

findOrganizationByToken :: String -> RequestContextM Organization
findOrganizationByToken token = createFindEntityByFn entityName [("token", token)]

findOrganizationByToken' :: String -> RequestContextM (Maybe Organization)
findOrganizationByToken' token = createFindEntityByFn' entityName [("token", token)]

findOrganizationByEmail :: String -> RequestContextM Organization
findOrganizationByEmail email = createFindEntityByFn entityName [("email", email)]

findOrganizationByEmail' :: String -> RequestContextM (Maybe Organization)
findOrganizationByEmail' email = createFindEntityByFn' entityName [("email", email)]

insertOrganization :: Organization -> RequestContextM Int64
insertOrganization = createInsertFn entityName

updateOrganization :: Organization -> RequestContextM Int64
updateOrganization org = do
  let sql =
        fromString
          "UPDATE organization SET organization_id = ?, name = ?, description = ?, email = ?, role = ?, token = ?, active = ?, logo = ?, created_at = ?, updated_at = ? WHERE organization_id = ?"
  let params = toRow org ++ [toField . T.pack $ org.organizationId]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteOrganizations :: RequestContextM Int64
deleteOrganizations = createDeleteEntitiesFn entityName

deleteOrganizationByOrgId :: String -> RequestContextM Int64
deleteOrganizationByOrgId organizationId = createDeleteEntityByFn entityName [("organization_id", organizationId)]
