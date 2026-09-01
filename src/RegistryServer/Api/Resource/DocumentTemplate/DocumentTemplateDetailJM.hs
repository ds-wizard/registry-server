module RegistryServer.Api.Resource.DocumentTemplate.DocumentTemplateDetailJM where

import Data.Aeson

import RegistryPublic.Api.Resource.Organization.OrganizationSimpleJM ()
import RegistryServer.Api.Resource.DocumentTemplate.DocumentTemplateDetailDTO
import Shared.Api.Resource.Common.SemVer2TupleJM ()
import Shared.Util.Aeson

instance FromJSON DocumentTemplateDetailDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateDetailDTO where
  toJSON = genericToJSON jsonOptions
