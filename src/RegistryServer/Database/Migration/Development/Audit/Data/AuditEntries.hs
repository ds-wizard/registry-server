module RegistryServer.Database.Migration.Development.Audit.Data.AuditEntries where

import Data.Maybe (fromJust)
import Data.Time

import RegistryPublic.Database.Migration.Development.Organization.Data.Organizations
import RegistryPublic.Model.Organization.Organization
import RegistryServer.Database.Migration.Development.Statistics.Data.InstanceStatistics
import RegistryServer.Model.Audit.AuditEntry
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.Model.Coordinate.Coordinate
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage ()

listPackagesAuditEntry :: AuditEntry
listPackagesAuditEntry =
  ListPackagesAuditEntry
    { organizationId = orgGlobal.organizationId
    , instanceStatistics = iStat
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    }

getKnowledgeModelBundleAuditEntry :: AuditEntry
getKnowledgeModelBundleAuditEntry =
  GetKnowledgeModelBundleAuditEntry
    { organizationId = orgGlobal.organizationId
    , knowledgeModelPackageId = show . createCoordinate $ netherlandsKmPackageV2
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    }
