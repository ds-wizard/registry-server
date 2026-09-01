module RegistryServer.Service.Organization.OrganizationMapper where

import Data.Time

import RegistryPublic.Api.Resource.Organization.OrganizationCreateDTO
import RegistryPublic.Api.Resource.Organization.OrganizationDTO
import RegistryPublic.Model.Organization.Organization
import RegistryPublic.Model.Organization.OrganizationRole
import RegistryPublic.Model.Organization.OrganizationSimple
import RegistryServer.Api.Resource.Organization.OrganizationChangeDTO

toDTO :: Organization -> OrganizationDTO
toDTO organization =
  OrganizationDTO
    { organizationId = organization.organizationId
    , name = organization.name
    , description = organization.description
    , email = organization.email
    , oRole = organization.oRole
    , token = organization.token
    , logo = organization.logo
    , active = organization.active
    , createdAt = organization.createdAt
    , updatedAt = organization.updatedAt
    }

toSimpleDTO :: Organization -> OrganizationSimple
toSimpleDTO organization =
  OrganizationSimple
    { organizationId = organization.organizationId
    , name = organization.name
    , logo = organization.logo
    }

fromCreateDTO :: OrganizationCreateDTO -> OrganizationRole -> String -> UTCTime -> UTCTime -> UTCTime -> Organization
fromCreateDTO dto orgRole orgToken orgCreatedAt orgUpdatedAt orgLastAccessAt =
  Organization
    { organizationId = dto.organizationId
    , name = dto.name
    , description = dto.description
    , email = dto.email
    , oRole = orgRole
    , token = orgToken
    , active = False
    , logo = Nothing
    , createdAt = orgCreatedAt
    , updatedAt = orgUpdatedAt
    }

fromChangeDTO :: OrganizationChangeDTO -> OrganizationDTO -> UTCTime -> Organization
fromChangeDTO dto org orgUpdatedAt =
  Organization
    { organizationId = org.organizationId
    , name = dto.name
    , description = dto.description
    , email = dto.email
    , oRole = org.oRole
    , token = org.token
    , active = org.active
    , logo = org.logo
    , createdAt = org.createdAt
    , updatedAt = orgUpdatedAt
    }
