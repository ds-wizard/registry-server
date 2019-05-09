module Service.KnowledgeModelBundle.KnowledgeModelBundleService
  ( exportKnowledgeModelBundle
  ) where

import Control.Lens ((^.))

import Api.Resource.KnowledgeModelBundle.KnowledgeModelBundleDTO
import Api.Resource.KnowledgeModelBundle.KnowledgeModelBundleJM ()
import Constant.KnowledgeModel
import LensesConfig
import Model.Context.AppContext
import Model.Error.Error
import Model.KnowledgeModelBundle.KnowledgeModelBundle
import Service.KnowledgeModelBundle.KnowledgeModelBundleMapper
import Service.Package.PackageService

exportKnowledgeModelBundle :: String -> AppContextM (Either AppError KnowledgeModelBundleDTO)
exportKnowledgeModelBundle kmbId =
  heGetSeriesOfPackages kmbId $ \packages -> do
    let newestPackage = last packages
    let kmb =
          KnowledgeModelBundle
          { _knowledgeModelBundleBundleId = newestPackage ^. pId
          , _knowledgeModelBundleName = newestPackage ^. name
          , _knowledgeModelBundleOrganizationId = newestPackage ^. organizationId
          , _knowledgeModelBundleKmId = newestPackage ^. kmId
          , _knowledgeModelBundleVersion = newestPackage ^. version
          , _knowledgeModelBundleMetamodelVersion = kmMetamodelVersion
          , _knowledgeModelBundlePackages = packages
          }
    return . Right . toDTO $ kmb
