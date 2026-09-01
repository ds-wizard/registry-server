module RegistryServer.Api.Resource.DocumentTemplate.DocumentTemplateDetailSM where

import Data.Swagger

import RegistryServer.Api.Resource.DocumentTemplate.DocumentTemplateDetailDTO
import RegistryServer.Api.Resource.DocumentTemplate.DocumentTemplateDetailJM ()
import RegistryServer.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleSM ()
import RegistryServer.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import Shared.Api.Resource.DocumentTemplate.DocumentTemplateSM ()
import Shared.Util.Swagger

instance ToSchema DocumentTemplateDetailDTO where
  declareNamedSchema = toSwagger wizardDocumentTemplateDetailDTO
