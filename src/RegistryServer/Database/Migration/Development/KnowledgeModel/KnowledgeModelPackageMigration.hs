module RegistryServer.Database.Migration.Development.KnowledgeModel.KnowledgeModelPackageMigration where

import Data.Foldable (traverse_)

import RegistryServer.Model.Context.ContextLenses ()
import RegistryServer.Model.Context.RequestContext
import Shared.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.Database.DAO.Package.KnowledgeModelPackageEventDAO
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.Util.Logger

runMigration :: RequestContextM ()
runMigration = do
  logInfo _CMP_MIGRATION "(Fixtures/KnowledgeModelPackage) started"
  deletePackages
  insertPackage globalKmPackageEmpty
  traverse_ insertPackageEvent globalKmPackageEmptyEvents
  insertPackage globalKmPackage
  traverse_ insertPackageEvent globalKmPackageEvents
  insertPackage netherlandsKmPackage
  traverse_ insertPackageEvent netherlandsKmPackageEvents
  insertPackage netherlandsKmPackageV2
  traverse_ insertPackageEvent netherlandsKmPackageV2Events
  logInfo _CMP_MIGRATION "(Fixtures/Package) ended"
