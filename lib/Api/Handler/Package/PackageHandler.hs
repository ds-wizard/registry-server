module Api.Handler.Package.PackageHandler where

import Web.Scotty.Trans (json, param)

import Api.Handler.Common
import Api.Resource.Package.PackageDTO ()
import Api.Resource.Package.PackageDetailJM ()
import Service.KnowledgeModelBundle.KnowledgeModelBundleService
import Service.Package.PackageService

getUniquePackagesA :: Endpoint
getUniquePackagesA =
  getAuthServiceExecutor $ \runInAuthService -> do
    queryParams <- getListOfQueryParamsIfPresent ["organizationId", "kmId"]
    eitherResDtos <- runInAuthService $ getSimplePackagesFiltered queryParams
    case eitherResDtos of
      Right resDtos -> json resDtos
      Left error -> sendError error

getPackageA :: Endpoint
getPackageA =
  getAuthServiceExecutor $ \runInAuthService -> do
    pkgId <- param "pkgId"
    eitherResDto <- runInAuthService $ getPackageById pkgId
    case eitherResDto of
      Right resDto -> json resDto
      Left error -> sendError error

getPackageExportA :: Endpoint
getPackageExportA =
  getAuthServiceExecutor $ \runInAuthService -> do
    pkgId <- param "pkgId"
    eitherResDto <- runInAuthService $ exportKnowledgeModelBundle pkgId
    case eitherResDto of
      Right resDto -> json resDto
      Left error -> sendError error
