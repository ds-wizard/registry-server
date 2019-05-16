module Api.Handler.Package.PackageHandler where

import Web.Scotty.Trans (json, param)

import Api.Handler.Common
import Api.Resource.Package.PackageDetailJM ()
import Api.Resource.Package.PackageSimpleJM ()
import Service.Package.PackageService
import Service.PackageBundle.PackageBundleService

getUniquePackagesA :: Endpoint
getUniquePackagesA = do
  queryParams <- getListOfQueryParamsIfPresent ["organizationId", "kmId"]
  eitherResDtos <- runInUnauthService $ getSimplePackagesFiltered queryParams
  case eitherResDtos of
    Right resDtos -> json resDtos
    Left error -> sendError error

getPackageA :: Endpoint
getPackageA = do
  pkgId <- param "pkgId"
  eitherResDto <- runInUnauthService $ getPackageById pkgId
  case eitherResDto of
    Right resDto -> json resDto
    Left error -> sendError error

getPackageExportA :: Endpoint
getPackageExportA =
  getAuthServiceExecutor $ \runInAuthService -> do
    pkgId <- param "pkgId"
    eitherResDto <- runInAuthService $ exportPackageBundle pkgId
    case eitherResDto of
      Right resDto -> json resDto
      Left error -> sendError error
