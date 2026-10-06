module RegistryServer.Database.Migration.Development.Audit.Data.AuditEntries where

import Data.Maybe (fromJust)
import Data.Time

import RegistryServer.Database.Migration.Development.Statistics.Data.InstanceStatistics
import RegistryServer.Database.Migration.Development.User.Data.Users
import RegistryServer.Model.Audit.AuditEntry
import RegistryServer.Model.User.User
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.Model.Coordinate.Coordinate
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage ()

listPackagesAuditEntry :: AuditEntry
listPackagesAuditEntry =
  ListPackagesAuditEntry
    { userUuid = Just userAdmin.uuid
    , instanceStatistics = iStat
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    }

getKnowledgeModelBundleAuditEntry :: AuditEntry
getKnowledgeModelBundleAuditEntry =
  GetKnowledgeModelBundleAuditEntry
    { userUuid = Just userAdmin.uuid
    , knowledgeModelPackageReference = show . createCoordinate $ netherlandsKmPackageV2
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    }
