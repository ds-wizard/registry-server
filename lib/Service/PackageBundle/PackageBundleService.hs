module Service.PackageBundle.PackageBundleService
  ( exportPackageBundle
  ) where

import Control.Lens ((^.))

import Api.Resource.PackageBundle.PackageBundleDTO
import Api.Resource.PackageBundle.PackageBundleJM ()
import Constant.KnowledgeModel
import LensesConfig
import Model.Context.AppContext
import Model.Error.Error
import Model.PackageBundle.PackageBundle
import Service.Package.PackageService
import Service.PackageBundle.PackageBundleMapper

exportPackageBundle :: String -> AppContextM (Either AppError PackageBundleDTO)
exportPackageBundle pbId =
  heGetSeriesOfPackages pbId $ \packages -> do
    let newestPackage = last packages
    let pb =
          PackageBundle
          { _packageBundleBundleId = newestPackage ^. pId
          , _packageBundleName = newestPackage ^. name
          , _packageBundleOrganizationId = newestPackage ^. organizationId
          , _packageBundleKmId = newestPackage ^. kmId
          , _packageBundleVersion = newestPackage ^. version
          , _packageBundleMetamodelVersion = kmMetamodelVersion
          , _packageBundlePackages = packages
          }
    return . Right . toDTO $ pb
