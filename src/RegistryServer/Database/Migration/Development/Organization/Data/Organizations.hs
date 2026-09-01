module RegistryServer.Database.Migration.Development.Organization.Data.Organizations where

import RegistryPublic.Database.Migration.Development.Organization.Data.Organizations
import RegistryPublic.Model.Organization.Organization
import RegistryServer.Api.Resource.Organization.OrganizationChangeDTO

orgGlobalEditedChange :: OrganizationChangeDTO
orgGlobalEditedChange =
  OrganizationChangeDTO
    { name = orgGlobalEdited.name
    , description = orgGlobalEdited.description
    , email = orgGlobalEdited.email
    }
