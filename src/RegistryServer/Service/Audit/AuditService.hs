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

import RegistryServer.Database.DAO.Audit.AuditEntryDAO
import RegistryServer.Model.Audit.AuditEntry
import RegistryServer.Model.Context.RequestContext
import RegistryServer.Model.Statistics.InstanceStatistics
import RegistryServer.Model.User.User
import Shared.Constant.Api
import Shared.Model.Coordinate.Coordinate
import Shared.Model.Error.Error

auditListPackages :: [(String, String)] -> RequestContextM (Either AppError (Maybe AuditEntry))
auditListPackages headers =
  heGetUserFromContext $ \user -> do
    now <- liftIO getCurrentTime
    let iStat = getInstanceStaticsFromHeaders headers
    let entry =
          ListPackagesAuditEntry {userUuid = Just user.uuid, instanceStatistics = iStat, createdAt = now}
    insertAuditEntry entry
    return . Right . Just $ entry

auditGetKnowledgeModelBundle :: Coordinate -> RequestContextM (Either AppError (Maybe AuditEntry))
auditGetKnowledgeModelBundle coordinate =
  heGetUserFromContext $ \user -> do
    now <- liftIO getCurrentTime
    let entry = GetKnowledgeModelBundleAuditEntry {userUuid = Just user.uuid, knowledgeModelPackageReference = show coordinate, createdAt = now}
    insertAuditEntry entry
    return . Right . Just $ entry

auditGetDocumentTemplateBundle :: Coordinate -> RequestContextM (Either AppError (Maybe AuditEntry))
auditGetDocumentTemplateBundle coordinate =
  heGetUserFromContext $ \user -> do
    now <- liftIO getCurrentTime
    let entry = GetDocumentTemplateBundleAuditEntry {userUuid = Just user.uuid, documentTemplateReference = show coordinate, createdAt = now}
    insertAuditEntry entry
    return . Right . Just $ entry

auditGetLocaleBundle :: Coordinate -> RequestContextM (Either AppError (Maybe AuditEntry))
auditGetLocaleBundle coordinate =
  heGetUserFromContext $ \user -> do
    now <- liftIO getCurrentTime
    let entry = GetLocaleBundleAuditEntry {userUuid = Just user.uuid, localeReference = show coordinate, createdAt = now}
    insertAuditEntry entry
    return . Right . Just $ entry

-- --------------------------------
-- PRIVATE
-- --------------------------------
heGetUserFromContext callback = do
  mUser <- asks (.currentUser)
  case mUser of
    Just user -> callback user
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
