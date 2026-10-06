module RegistryServer.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleSM where

import Data.Swagger

import RegistryPublic.Api.Resource.Package.KnowledgeModelPackageSimpleDTO
import RegistryPublic.Api.Resource.Package.KnowledgeModelPackageSimpleJM ()
import RegistryServer.Service.KnowledgeModel.Package.KnowledgeModelPackageMapper
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.Util.Swagger

instance ToSchema KnowledgeModelPackageSimpleDTO where
  declareNamedSchema = toSwagger (toSimpleDTO globalKmPackage)
