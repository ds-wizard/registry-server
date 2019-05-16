module Database.DAO.Package.PackageDAO where

import Data.Bson
import Data.Text (Text)
import Database.MongoDB ((=:))
import Model.Error.Error

import Database.BSON.Package.Package ()
import Database.BSON.Package.PackageWithEvents ()
import Database.DAO.Common
import Model.Context.AppContext
import Model.Package.Package
import Model.Package.PackageWithEvents
import Util.Helper (createHeeHelper, createHemHelper)

entityName = "package"

collection = "packages"

findPackages :: AppContextM (Either AppError [Package])
findPackages = createFindEntitiesFn collection

findPackageWithEvents :: AppContextM (Either AppError [PackageWithEvents])
findPackageWithEvents = createFindEntitiesFn collection

findPackagesFiltered :: [(Text, Text)] -> AppContextM (Either AppError [Package])
findPackagesFiltered queryParams = createFindEntitiesByFn collection (mapToDBQueryParams queryParams)

findPackagesByKmId :: String -> AppContextM (Either AppError [Package])
findPackagesByKmId kmId = createFindEntitiesByFn collection ["kmId" =: kmId]

findPackagesByOrganizationIdAndKmId :: String -> String -> AppContextM (Either AppError [Package])
findPackagesByOrganizationIdAndKmId organizationId kmId =
  createFindEntitiesByFn collection ["organizationId" =: organizationId, "kmId" =: kmId]

findPackagesByParentPackageId :: String -> AppContextM (Either AppError [Package])
findPackagesByParentPackageId parentPackageId = createFindEntitiesByFn collection ["parentPackageId" =: parentPackageId]

findPackageById :: String -> AppContextM (Either AppError Package)
findPackageById = createFindEntityByFn collection entityName "id"

findPackageWithEventsById :: String -> AppContextM (Either AppError PackageWithEvents)
findPackageWithEventsById = createFindEntityByFn collection entityName "id"

insertPackage :: PackageWithEvents -> AppContextM Value
insertPackage = createInsertFn collection

deletePackages :: AppContextM ()
deletePackages = createDeleteEntitiesFn collection

deletePackagesFiltered :: [(Text, Text)] -> AppContextM ()
deletePackagesFiltered queryParams = createDeleteEntitiesByFn collection (mapToDBQueryParams queryParams)

deletePackageById :: String -> AppContextM ()
deletePackageById = createDeleteEntityByFn collection "organizationId"

-- --------------------------------
-- HELPERS
-- --------------------------------
heFindPackages callback = createHeeHelper findPackages callback

-- --------------------------------
heFindPackagesFiltered queryParams callback = createHeeHelper (findPackagesFiltered queryParams) callback

hmFindPackagesFiltered queryParams callback = createHemHelper (findPackagesFiltered queryParams) callback

-- --------------------------------
heFindPackagesByOrganizationIdAndKmId organizationId kmId callback =
  createHeeHelper (findPackagesByOrganizationIdAndKmId organizationId kmId) callback

-- --------------------------------
heFindPackageById pkgId callback = createHeeHelper (findPackageById pkgId) callback

hmFindPackageById pkgId callback = createHemHelper (findPackageById pkgId) callback

-- --------------------------------
heFindPackageWithEventsById pkgId callback = createHeeHelper (findPackageWithEventsById pkgId) callback
