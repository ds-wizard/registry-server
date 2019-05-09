module Service.Package.PackageService
  ( getSimplePackagesFiltered
  , getPackageById
  , getSeriesOfPackages
  -- Helpers
  , heGetSeriesOfPackages
  ) where

import Control.Lens ((&), (.~), (^.))
import Data.List
import Data.Maybe
import Data.Text (Text)

import Api.Resource.Package.PackageDetailDTO
import Api.Resource.Package.PackageSimpleDTO
import Database.DAO.Package.PackageDAO
import LensesConfig
import Model.Context.AppContext
import Model.Error.Error
import Model.Package.Package
import Service.Organization.OrganizationService
import Service.Package.PackageMapper
import Service.Package.PackageValidation
import Util.Helper (createHeeHelper)

getSimplePackagesFiltered :: [(Text, Text)] -> AppContextM (Either AppError [PackageSimpleDTO])
getSimplePackagesFiltered queryParams = do
  heFindPackagesFiltered queryParams $ \packages -> do
    let uniquePackages = foldl addIfUnique [] packages
    return . Right $ uniquePackages
  where
    addIfUnique :: [PackageSimpleDTO] -> Package -> [PackageSimpleDTO]
    addIfUnique packageDtos newPackage =
      case isAlreadyInArray packageDtos newPackage of
        (Just packageDto) ->
          let withoutDesiredPackage = delete packageDto packageDtos
              updatedPackageDto = computeLatestVersion packageDto newPackage
          in withoutDesiredPackage ++ [updatedPackageDto]
        Nothing -> packageDtos ++ [packageToSimpleDTO newPackage]
    isAlreadyInArray :: [PackageSimpleDTO] -> Package -> Maybe PackageSimpleDTO
    isAlreadyInArray packageDtos newPackage =
      find
        (\pkg -> (newPackage ^. kmId) == (pkg ^. kmId) && (newPackage ^. organizationId) == (pkg ^. organizationId))
        packageDtos
    computeLatestVersion :: PackageSimpleDTO -> Package -> PackageSimpleDTO
    computeLatestVersion packageDto newPackage =
      let originalVersion = packageDto ^. latestVersion
          newVersion = newPackage ^. version
      in if isNothing $ validateIsVersionHigher newVersion originalVersion
           then packageDto & latestVersion .~ newVersion
           else packageDto

getPackageById :: String -> AppContextM (Either AppError PackageDetailDTO)
getPackageById pkgId =
  heFindPackageById pkgId $ \pkg ->
    heFindPackagesByOrganizationIdAndKmId (pkg ^. organizationId) (pkg ^. kmId) $ \allPkgs ->
      heGetOrganizationByOrgId (pkg ^. organizationId) $ \org ->
        return . Right $ packageToDetailDTO pkg (_packageVersion <$> allPkgs) org

getSeriesOfPackages :: String -> AppContextM (Either AppError [PackageWithEvents])
getSeriesOfPackages pkgId =
  heFindPackageWithEventsById pkgId $ \package ->
    case package ^. parentPackageId of
      Just parentPkgId ->
        heGetSeriesOfPackages parentPkgId $ \parentPackages -> return . Right $ parentPackages ++ [package]
      Nothing -> return . Right $ [package]

-- --------------------------------
-- HELPERS
-- --------------------------------
heGetSeriesOfPackages pkgId callback = createHeeHelper (getSeriesOfPackages pkgId) callback
