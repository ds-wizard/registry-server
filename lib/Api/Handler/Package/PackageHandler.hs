module Api.Handler.Package.PackageHandler where

import Web.Scotty.Trans (json, param)

import Api.Handler.Common
import Api.Resource.Package.PackageDetailJM ()
import Api.Resource.Package.PackageSimpleJM ()
import Service.Package.PackageService
import Service.PackageBundle.PackageBundleService

getPackagesA :: Endpoint
getPackagesA =
  getMaybeAuthServiceExecutor $ \runInMaybeAuthService -> do
    queryParams <- getListOfQueryParamsIfPresent ["organizationId", "kmId"]
    eitherResDtos <- runInMaybeAuthService $ getSimplePackagesFiltered queryParams
    case eitherResDtos of
      Right resDtos -> json resDtos
      Left error -> sendError error

getPackageA :: Endpoint
getPackageA =
  getMaybeAuthServiceExecutor $ \runInMaybeAuthService -> do
    pkgId <- param "pkgId"
    eitherResDto <- runInMaybeAuthService $ getPackageById pkgId
    case eitherResDto of
      Right resDto -> json resDto
      Left error -> sendError error

getPackageBundleA :: Endpoint
getPackageBundleA =
  getAuthServiceExecutor $ \runInAuthService -> do
    pkgId <- param "pkgId"
    eitherResDto <- runInAuthService $ getPackageBundle pkgId
    case eitherResDto of
      Right resDto -> json resDto
      Left error -> sendError error
