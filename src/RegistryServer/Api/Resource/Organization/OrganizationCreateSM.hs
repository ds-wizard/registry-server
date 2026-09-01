module RegistryServer.Api.Resource.Organization.OrganizationCreateSM where

import Data.Swagger

import RegistryPublic.Api.Resource.Organization.OrganizationCreateDTO
import RegistryPublic.Api.Resource.Organization.OrganizationCreateJM ()
import RegistryPublic.Database.Migration.Development.Organization.Data.Organizations
import Shared.Util.Swagger

instance ToSchema OrganizationCreateDTO where
  declareNamedSchema = toSwagger orgGlobalCreate
