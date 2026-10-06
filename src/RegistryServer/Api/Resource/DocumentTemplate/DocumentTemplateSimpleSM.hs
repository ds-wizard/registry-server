module RegistryServer.Api.Resource.DocumentTemplate.DocumentTemplateSimpleSM where

import Data.Swagger

import RegistryPublic.Api.Resource.DocumentTemplate.DocumentTemplateSimpleDTO
import RegistryPublic.Api.Resource.DocumentTemplate.DocumentTemplateSimpleJM ()
import RegistryServer.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import Shared.Util.Swagger

instance ToSchema DocumentTemplateSimpleDTO where
  declareNamedSchema = toSwagger wizardDocumentTemplateSimpleDTO
