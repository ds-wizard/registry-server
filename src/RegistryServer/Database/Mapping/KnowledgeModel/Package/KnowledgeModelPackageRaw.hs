module RegistryServer.Database.Mapping.KnowledgeModel.Package.KnowledgeModelPackageRaw where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow

import RegistryServer.Model.KnowledgeModel.Package.KnowledgeModelPackageRaw
import Shared.Database.Mapping.Coordinate.Coordinate
import Shared.Database.Mapping.KnowledgeModel.Package.KnowledgeModelPackagePhase ()

instance FromRow KnowledgeModelPackageRaw where
  fromRow = do
    id <- field
    name <- field
    version <- field
    phase <- field
    metamodelVersion <- field
    description <- field
    readme <- field
    license <- field
    previousPackageId <- coordinateFromFields
    forkOfPackageId <- coordinateFromFields
    mergeCheckpointPackageId <- coordinateFromFields
    events <- fieldWith fromJSONField
    nonEditable <- field
    createdAt <- field
    language <- field
    return $ KnowledgeModelPackageRaw {..}
