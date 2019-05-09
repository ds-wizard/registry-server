module Database.Migration.Development.Migration
  ( runMigration
  ) where

import Constant.Component
import qualified
       Database.Migration.Development.Organization.OrganizationMigration
       as ORG
import qualified
       Database.Migration.Development.Package.PackageMigration as PKG
import Util.Logger

runMigration = do
  logInfo $ msg _CMP_MIGRATION "started"
  ORG.runMigration
  PKG.runMigration
  logInfo $ msg _CMP_MIGRATION "ended"
