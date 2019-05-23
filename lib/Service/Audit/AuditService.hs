module Service.Audit.AuditService
  ( auditListPackages
  , auditGetPackageBundle
  -- Helpers
  , heAuditListPackages
  , heAuditGetPackageBundle
  ) where

import Control.Lens ((^.))
import Control.Monad.Reader (asks, liftIO)
import Data.Time

import Database.DAO.Audit.AuditEntryDAO
import LensesConfig
import Model.Audit.AuditEntry
import Model.Context.AppContext
import Model.Error.Error
import Util.Helper (createHeeHelper)

auditListPackages :: AppContextM (Either AppError (Maybe AuditEntry))
auditListPackages =
  heGetOrganizationFromContext $ \org -> do
    now <- liftIO getCurrentTime
    let entry =
          ListPackagesAuditEntry' $
          ListPackagesAuditEntry
          {_listPackagesAuditEntryOrganizationId = org ^. organizationId, _listPackagesAuditEntryCreatedAt = now}
    insertAuditEntry entry
    return . Right . Just $ entry

auditGetPackageBundle :: String -> AppContextM (Either AppError (Maybe AuditEntry))
auditGetPackageBundle pkgId =
  heGetOrganizationFromContext $ \org -> do
    now <- liftIO getCurrentTime
    let entry =
          GetPackageBundleAuditEntry' $
          GetPackageBundleAuditEntry
          { _getPackageBundleAuditEntryOrganizationId = org ^. organizationId
          , _getPackageBundleAuditEntryPackageId = pkgId
          , _getPackageBundleAuditEntryCreatedAt = now
          }
    insertAuditEntry entry
    return . Right . Just $ entry

-- --------------------------------
-- PRIVATE
-- --------------------------------
heGetOrganizationFromContext callback = do
  mOrg <- asks _appContextCurrentOrganization
  case mOrg of
    Just org -> callback org
    Nothing -> return . Right $ Nothing

-- --------------------------------
-- HELPERS
-- --------------------------------
heAuditListPackages callback = createHeeHelper auditListPackages callback

-- -----------------------------------------------------
heAuditGetPackageBundle pkgId callback = createHeeHelper (auditGetPackageBundle pkgId) callback
