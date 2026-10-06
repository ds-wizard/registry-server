module RegistryServer.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageDetailJM where

import Data.Aeson

import RegistryServer.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageDetailDTO
import Shared.Api.Resource.Coordinate.CoordinateJM ()
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackagePhaseJM ()
import Shared.Util.Aeson

instance FromJSON KnowledgeModelPackageDetailDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON KnowledgeModelPackageDetailDTO where
  toJSON = genericToJSON jsonOptions
