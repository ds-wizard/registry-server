module RegistryServer.Database.DAO.Audit.AuditEntryDAO where

import GHC.Int

import RegistryServer.Database.DAO.Common
import RegistryServer.Database.Mapping.Audit.AuditEntry ()
import RegistryServer.Model.Audit.AuditEntry
import RegistryServer.Model.Context.ContextLenses ()
import RegistryServer.Model.Context.RequestContext

entityName = "audit"

findAuditEntries :: RequestContextM [AuditEntry]
findAuditEntries = createFindEntitiesFn entityName

insertAuditEntry :: AuditEntry -> RequestContextM Int64
insertAuditEntry = createInsertFn entityName

deleteAuditEntries :: RequestContextM Int64
deleteAuditEntries = createDeleteEntitiesFn entityName
