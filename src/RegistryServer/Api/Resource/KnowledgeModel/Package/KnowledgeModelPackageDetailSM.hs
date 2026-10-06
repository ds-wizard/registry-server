module RegistryServer.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageDetailSM where

import Data.Swagger

import RegistryServer.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageDetailDTO
import RegistryServer.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageDetailJM ()
import RegistryServer.Service.KnowledgeModel.Package.KnowledgeModelPackageMapper
import Shared.Api.Resource.Coordinate.CoordinateSM ()
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackagePhaseSM ()
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.Util.Swagger

instance ToSchema KnowledgeModelPackageDetailDTO where
  declareNamedSchema = toSwagger (toDetailDTO globalKmPackage [])
