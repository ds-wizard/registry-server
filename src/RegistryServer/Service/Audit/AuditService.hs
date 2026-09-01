module RegistryServer.Service.Audit.AuditService (
  auditListPackages,
  auditGetKnowledgeModelBundle,
  auditGetDocumentTemplateBundle,
  auditGetLocaleBundle,
) where

import Control.Monad.Reader (asks, liftIO)
import Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Time
import Text.Read (readMaybe)

import RegistryPublic.Model.Organization.Organization
import RegistryServer.Database.DAO.Audit.AuditEntryDAO
import RegistryServer.Model.Audit.AuditEntry
import RegistryServer.Model.Context.RequestContext
import RegistryServer.Model.Statistics.InstanceStatistics
import Shared.Constant.Api
import Shared.Model.Coordinate.Coordinate
import Shared.Model.Error.Error

auditListPackages :: [(String, String)] -> RequestContextM (Either AppError (Maybe AuditEntry))
auditListPackages headers =
  heGetOrganizationFromContext $ \org -> do
    now <- liftIO getCurrentTime
    let iStat = getInstanceStaticsFromHeaders headers
    let entry =
          ListPackagesAuditEntry {organizationId = org.organizationId, instanceStatistics = iStat, createdAt = now}
    insertAuditEntry entry
    return . Right . Just $ entry

auditGetKnowledgeModelBundle :: Coordinate -> RequestContextM (Either AppError (Maybe AuditEntry))
auditGetKnowledgeModelBundle coordinate =
  heGetOrganizationFromContext $ \org -> do
    now <- liftIO getCurrentTime
    let entry = GetKnowledgeModelBundleAuditEntry {organizationId = org.organizationId, knowledgeModelPackageId = show coordinate, createdAt = now}
    insertAuditEntry entry
    return . Right . Just $ entry

auditGetDocumentTemplateBundle :: Coordinate -> RequestContextM (Either AppError (Maybe AuditEntry))
auditGetDocumentTemplateBundle coordinate =
  heGetOrganizationFromContext $ \org -> do
    now <- liftIO getCurrentTime
    let entry = GetDocumentTemplateBundleAuditEntry {organizationId = org.organizationId, documentTemplateId = show coordinate, createdAt = now}
    insertAuditEntry entry
    return . Right . Just $ entry

auditGetLocaleBundle :: Coordinate -> RequestContextM (Either AppError (Maybe AuditEntry))
auditGetLocaleBundle coordinate =
  heGetOrganizationFromContext $ \org -> do
    now <- liftIO getCurrentTime
    let entry = GetLocaleBundleAuditEntry {organizationId = org.organizationId, localeId = show coordinate, createdAt = now}
    insertAuditEntry entry
    return . Right . Just $ entry

-- --------------------------------
-- PRIVATE
-- --------------------------------
heGetOrganizationFromContext callback = do
  mOrg <- asks currentOrganization
  case mOrg of
    Just org -> callback org
    Nothing -> return . Right $ Nothing

-- -----------------------------------------------------
getInstanceStaticsFromHeaders headers =
  let get key = fromMaybe (-1) (M.lookup key (M.fromList headers) >>= readMaybe)
   in InstanceStatistics
        { userCount = get xUserCountHeaderName
        , pkgCount = get xKnowledgeModelPackageCountHeaderName
        , prjCount = get xProjectCountHeaderName
        , kmEditorCount = get xKnowledgeModelEditorCountHeaderName
        , docCount = get xDocCountHeaderName
        , tmlCount = get xTmlCountHeaderName
        }
