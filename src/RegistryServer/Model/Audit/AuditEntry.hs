module RegistryServer.Model.Audit.AuditEntry where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import RegistryServer.Model.Statistics.InstanceStatistics

data AuditEntry
  = ListPackagesAuditEntry
      { userUuid :: Maybe U.UUID
      , instanceStatistics :: InstanceStatistics
      , createdAt :: UTCTime
      }
  | GetKnowledgeModelBundleAuditEntry
      { userUuid :: Maybe U.UUID
      , knowledgeModelPackageReference :: String
      , createdAt :: UTCTime
      }
  | GetDocumentTemplateBundleAuditEntry
      { userUuid :: Maybe U.UUID
      , documentTemplateReference :: String
      , createdAt :: UTCTime
      }
  | GetLocaleBundleAuditEntry
      { userUuid :: Maybe U.UUID
      , localeReference :: String
      , createdAt :: UTCTime
      }
  deriving (Show, Generic)

instance Eq AuditEntry where
  ae1@ListPackagesAuditEntry {} == ae2@ListPackagesAuditEntry {} =
    ae1.userUuid == ae2.userUuid
      && ae1.instanceStatistics == ae2.instanceStatistics
  ae1@GetKnowledgeModelBundleAuditEntry {} == ae2@GetKnowledgeModelBundleAuditEntry {} =
    ae1.userUuid == ae2.userUuid
      && ae1.knowledgeModelPackageReference == ae2.knowledgeModelPackageReference
  ae1@GetDocumentTemplateBundleAuditEntry {} == ae2@GetDocumentTemplateBundleAuditEntry {} =
    ae1.userUuid == ae2.userUuid
      && ae1.documentTemplateReference == ae2.documentTemplateReference
  ae1@GetLocaleBundleAuditEntry {} == ae2@GetLocaleBundleAuditEntry {} =
    ae1.userUuid == ae2.userUuid
      && ae1.localeReference == ae2.localeReference
  _ == _ = False
