module RegistryServer.Api.Resource.Organization.OrganizationStateSM where

import Data.Swagger

import RegistryPublic.Api.Resource.Organization.OrganizationStateDTO
import RegistryPublic.Api.Resource.Organization.OrganizationStateJM ()
import RegistryPublic.Database.Migration.Development.Organization.Data.Organizations
import Shared.Util.Swagger

instance ToSchema OrganizationStateDTO where
  declareNamedSchema = toSwagger orgStateDto
