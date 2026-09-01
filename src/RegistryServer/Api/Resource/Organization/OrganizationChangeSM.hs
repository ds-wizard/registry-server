module RegistryServer.Api.Resource.Organization.OrganizationChangeSM where

import Data.Swagger

import RegistryServer.Api.Resource.Organization.OrganizationChangeDTO
import RegistryServer.Api.Resource.Organization.OrganizationChangeJM ()
import RegistryServer.Database.Migration.Development.Organization.Data.Organizations
import Shared.Util.Swagger

instance ToSchema OrganizationChangeDTO where
  declareNamedSchema = toSwagger orgGlobalEditedChange
