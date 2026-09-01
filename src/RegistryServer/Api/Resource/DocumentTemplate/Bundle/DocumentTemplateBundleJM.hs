module RegistryServer.Api.Resource.DocumentTemplate.Bundle.DocumentTemplateBundleJM where

import Data.Aeson

import RegistryServer.Api.Resource.DocumentTemplate.DocumentTemplateJM ()
import Shared.Api.Resource.Common.SemVer2TupleJM ()
import Shared.Api.Resource.DocumentTemplateBundle.DocumentTemplateBundleDTO
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackagePattern
import Shared.Util.Aeson

instance ToJSON DocumentTemplateBundleDTO where
  toJSON DocumentTemplateBundleDTO {..} =
    object
      [ "id" .= tId
      , "name" .= name
      , "organizationId" .= organizationId
      , "templateId" .= templateId
      , "version" .= version
      , "metamodelVersion" .= metamodelVersion
      , "description" .= description
      , "readme" .= readme
      , "license" .= license
      , "allowedPackages" .= allowedPackages
      , "language" .= language
      , "recommendedPackageId" .= Null
      , "formats" .= formats
      , "files" .= files
      , "assets" .= assets
      , "createdAt" .= createdAt
      ]

instance ToJSON KnowledgeModelPackagePattern where
  toJSON = genericToJSON jsonOptions
