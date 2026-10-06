module RegistryServer.Api.Resource.KnowledgeModel.Bundle.KnowledgeModelBundleJM where

import Control.Monad
import Data.Aeson

import RegistryServer.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageRawJM ()
import RegistryServer.Model.KnowledgeModel.Bundle.KnowledgeModelBundle
import Shared.Api.Resource.Coordinate.CoordinateJM
import Shared.Util.Aeson

instance FromJSON KnowledgeModelBundle where
  parseJSON (Object o) = do
    id <- parseLegacyId o "kmId"
    name <- o .: "name"
    version <- o .: "version"
    metamodelVersion <- o .: "metamodelVersion"
    packages <- o .: "packages"
    return KnowledgeModelBundle {..}
  parseJSON _ = mzero

instance ToJSON KnowledgeModelBundle where
  toJSON = genericToJSON jsonOptions
