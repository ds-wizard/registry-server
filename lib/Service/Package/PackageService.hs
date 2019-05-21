module Service.Package.PackageService
  ( getSimplePackagesFiltered
  , getPackageById
  , getSeriesOfPackages
  -- Helpers
  , heGetSeriesOfPackages
  ) where

import Control.Lens ((^.))
import Data.List
import Data.Text (Text)

import Api.Resource.Package.PackageDetailDTO
import Api.Resource.Package.PackageSimpleDTO
import Database.DAO.Organization.OrganizationDAO
import Database.DAO.Package.PackageDAO
import LensesConfig
import Model.Context.AppContext
import Model.Error.Error
import Model.Package.Package
import Model.Package.PackageWithEvents
import Service.Package.PackageMapper
import Util.Helper (createHeeHelper)
import Util.List (foldEithersInContext)

getSimplePackagesFiltered :: [(Text, Text)] -> AppContextM (Either AppError [PackageSimpleDTO])
getSimplePackagesFiltered queryParams =
  heFindPackagesFiltered queryParams $ \pkgs ->
    foldEithersInContext . mapToSimpleDTO . chooseTheNewest . groupPkgs $ pkgs
  where
    groupPkgs :: [Package] -> [[Package]]
    groupPkgs = groupBy (\p1 p2 -> (p1 ^. organizationId) == (p2 ^. organizationId) && (p1 ^. kmId) == (p2 ^. kmId))
    chooseTheNewest :: [[Package]] -> [Package]
    chooseTheNewest = fmap (maximumBy (\p1 p2 -> compare (p1 ^. version) (p2 ^. version)))
    mapToSimpleDTO :: [Package] -> [AppContextM (Either AppError PackageSimpleDTO)]
    mapToSimpleDTO =
      fmap (\pkg -> heFindOrganizationByOrgId (pkg ^. organizationId) $ \org -> return . Right $ toSimpleDTO pkg org)

getPackageById :: String -> AppContextM (Either AppError PackageDetailDTO)
getPackageById pkgId =
  heFindPackageById pkgId $ \pkg ->
    heGetPackageVersions pkg $ \versions ->
      heFindOrganizationByOrgId (pkg ^. organizationId) $ \org -> return . Right $ toDetailDTO pkg versions org

getSeriesOfPackages :: String -> AppContextM (Either AppError [PackageWithEvents])
getSeriesOfPackages pkgId =
  heFindPackageWithEventsById pkgId $ \package ->
    case package ^. parentPackageId of
      Just parentPkgId ->
        heGetSeriesOfPackages parentPkgId $ \parentPackages -> return . Right $ parentPackages ++ [package]
      Nothing -> return . Right $ [package]

-- --------------------------------
-- PRIVATE
-- --------------------------------
getPackageVersions :: Package -> AppContextM (Either AppError [String])
getPackageVersions pkg =
  heFindPackagesByOrganizationIdAndKmId (pkg ^. organizationId) (pkg ^. kmId) $ \allPkgs ->
    return . Right . fmap _packageVersion $ allPkgs

-- --------------------------------
-- HELPERS
-- --------------------------------
heGetSeriesOfPackages pkgId callback = createHeeHelper (getSeriesOfPackages pkgId) callback

heGetPackageVersions pkg callback = createHeeHelper (getPackageVersions pkg) callback
