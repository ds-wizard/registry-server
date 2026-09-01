module RegistryServer.Service.KnowledgeModel.Package.KnowledgeModelPackageMapper where

import RegistryPublic.Api.Resource.Package.KnowledgeModelPackageSimpleDTO
import qualified RegistryPublic.Model.Organization.Organization as Organization
import RegistryServer.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageDetailDTO
import qualified RegistryServer.Service.Organization.OrganizationMapper as OM
import Shared.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventJM ()
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage

toSimpleDTO :: KnowledgeModelPackage -> Organization.Organization -> KnowledgeModelPackageSimpleDTO
toSimpleDTO pkg org =
  KnowledgeModelPackageSimpleDTO
    { uuid = pkg.uuid
    , name = pkg.name
    , organizationId = pkg.organizationId
    , kmId = pkg.kmId
    , version = pkg.version
    , description = pkg.description
    , createdAt = pkg.createdAt
    , organization = OM.toSimpleDTO org
    , language = pkg.language
    }

toDetailDTO :: KnowledgeModelPackage -> [String] -> Organization.Organization -> KnowledgeModelPackageDetailDTO
toDetailDTO pkg versions org =
  KnowledgeModelPackageDetailDTO
    { uuid = pkg.uuid
    , name = pkg.name
    , organizationId = pkg.organizationId
    , kmId = pkg.kmId
    , version = pkg.version
    , phase = pkg.phase
    , description = pkg.description
    , readme = pkg.readme
    , license = pkg.license
    , language = pkg.language
    , metamodelVersion = pkg.metamodelVersion
    , previousPackageUuid = pkg.previousPackageUuid
    , forkOfPackageId = pkg.forkOfPackageId
    , mergeCheckpointPackageId = pkg.mergeCheckpointPackageId
    , versions = versions
    , organization = OM.toSimpleDTO org
    , createdAt = pkg.createdAt
    }
