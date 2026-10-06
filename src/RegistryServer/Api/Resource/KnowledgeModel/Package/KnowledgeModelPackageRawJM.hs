module RegistryServer.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageRawJM where

import Control.Monad
import Data.Aeson

import RegistryServer.Model.KnowledgeModel.Package.KnowledgeModelPackageRaw
import Shared.Api.Resource.Coordinate.CoordinateJM
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackagePhaseJM ()

instance ToJSON KnowledgeModelPackageRaw where
  toJSON pkg =
    object $
      [ "id" .= pkg.id
      , "name" .= pkg.name
      , "version" .= pkg.version
      , "phase" .= pkg.phase
      , "metamodelVersion" .= pkg.metamodelVersion
      , "description" .= pkg.description
      , "readme" .= pkg.readme
      , "license" .= pkg.license
      , "language" .= pkg.language
      , "events" .= pkg.events
      , "nonEditable" .= pkg.nonEditable
      , "createdAt" .= pkg.createdAt
      ]
        ++ coordinateToPairs "previousPackage" pkg.previousPackageId
        ++ coordinateToPairs "forkOfPackage" pkg.forkOfPackageId
        ++ coordinateToPairs "mergeCheckpointPackage" pkg.mergeCheckpointPackageId

instance FromJSON KnowledgeModelPackageRaw where
  parseJSON (Object o) = do
    id <- parseLegacyId o "kmId"
    name <- o .: "name"
    version <- o .: "version"
    phase <- o .: "phase"
    metamodelVersion <- o .: "metamodelVersion"
    description <- o .: "description"
    readme <- o .: "readme"
    license <- o .: "license"
    language <- o .:? "language" .!= "en"
    previousPackageId <- parseCoordinateFields o "previousPackage"
    forkOfPackageId <- parseCoordinateFields o "forkOfPackage"
    mergeCheckpointPackageId <- parseCoordinateFields o "mergeCheckpointPackage"
    events <- o .: "events"
    nonEditable <- o .: "nonEditable"
    createdAt <- o .: "createdAt"
    return KnowledgeModelPackageRaw {..}
  parseJSON _ = mzero
