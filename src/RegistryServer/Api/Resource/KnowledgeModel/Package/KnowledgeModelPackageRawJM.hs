module RegistryServer.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageRawJM where

import Control.Monad
import Data.Aeson

import RegistryServer.Model.KnowledgeModel.Package.KnowledgeModelPackageRaw
import Shared.Api.Resource.Coordinate.CoordinateJM ()
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackagePhaseJM ()
import Shared.Util.Aeson

instance ToJSON KnowledgeModelPackageRaw where
  toJSON = genericToJSON jsonOptions

instance FromJSON KnowledgeModelPackageRaw where
  parseJSON (Object o) = do
    pId <- o .: "id"
    name <- o .: "name"
    organizationId <- o .: "organizationId"
    kmId <- o .: "kmId"
    version <- o .: "version"
    phase <- o .: "phase"
    metamodelVersion <- o .: "metamodelVersion"
    description <- o .: "description"
    readme <- o .: "readme"
    license <- o .: "license"
    language <- o .:? "language" .!= "en"
    previousPackageId <- o .:? "previousPackageId"
    forkOfPackageId <- o .:? "forkOfPackageId"
    mergeCheckpointPackageId <- o .:? "mergeCheckpointPackageId"
    events <- o .: "events"
    nonEditable <- o .: "nonEditable"
    createdAt <- o .: "createdAt"
    return KnowledgeModelPackageRaw {..}
  parseJSON _ = mzero
