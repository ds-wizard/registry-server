module RegistryServer.Service.KnowledgeModel.Package.KnowledgeModelPackageMapper where

import RegistryPublic.Api.Resource.Package.KnowledgeModelPackageSimpleDTO
import RegistryServer.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageDetailDTO
import Shared.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventJM ()
import Shared.Model.Coordinate.Coordinate
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage

toSimpleDTO :: KnowledgeModelPackage -> KnowledgeModelPackageSimpleDTO
toSimpleDTO pkg =
  KnowledgeModelPackageSimpleDTO
    { uuid = pkg.uuid
    , name = pkg.name
    , id = pkg.id
    , version = pkg.version
    , description = pkg.description
    , createdAt = pkg.createdAt
    , language = pkg.language
    }

toDetailDTO :: KnowledgeModelPackage -> [String] -> KnowledgeModelPackageDetailDTO
toDetailDTO pkg versions =
  KnowledgeModelPackageDetailDTO
    { uuid = pkg.uuid
    , name = pkg.name
    , id = pkg.id
    , version = pkg.version
    , phase = pkg.phase
    , description = pkg.description
    , readme = pkg.readme
    , license = pkg.license
    , language = pkg.language
    , metamodelVersion = pkg.metamodelVersion
    , previousPackageUuid = pkg.previousPackageUuid
    , forkOfPackageId = fmap (.id) pkg.forkOfPackageId
    , forkOfPackageVersion = fmap (.version) pkg.forkOfPackageId
    , mergeCheckpointPackageId = fmap (.id) pkg.mergeCheckpointPackageId
    , mergeCheckpointPackageVersion = fmap (.version) pkg.mergeCheckpointPackageId
    , versions = versions
    , createdAt = pkg.createdAt
    }
