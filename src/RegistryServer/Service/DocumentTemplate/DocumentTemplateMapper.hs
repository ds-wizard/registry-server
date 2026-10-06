module RegistryServer.Service.DocumentTemplate.DocumentTemplateMapper where

import RegistryPublic.Api.Resource.DocumentTemplate.DocumentTemplateSimpleDTO
import RegistryServer.Api.Resource.DocumentTemplate.DocumentTemplateDetailDTO
import Shared.Model.DocumentTemplate.DocumentTemplate

toSimpleDTO :: DocumentTemplate -> DocumentTemplateSimpleDTO
toSimpleDTO dt =
  DocumentTemplateSimpleDTO
    { uuid = dt.uuid
    , name = dt.name
    , id = dt.id
    , version = dt.version
    , description = dt.description
    , createdAt = dt.createdAt
    }

toDetailDTO :: DocumentTemplate -> [String] -> DocumentTemplateDetailDTO
toDetailDTO dt versions =
  DocumentTemplateDetailDTO
    { uuid = dt.uuid
    , name = dt.name
    , id = dt.id
    , version = dt.version
    , metamodelVersion = dt.metamodelVersion
    , description = dt.description
    , readme = dt.readme
    , license = dt.license
    , versions = versions
    , createdAt = dt.createdAt
    }
