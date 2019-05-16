module Service.Organization.OrganizationMapper where

import Control.Lens ((^.))
import Data.Time

import Api.Resource.Organization.OrganizationChangeDTO
import Api.Resource.Organization.OrganizationCreateDTO
import Api.Resource.Organization.OrganizationDTO
import Api.Resource.Organization.OrganizationSimpleDTO
import LensesConfig
import Model.Organization.Organization

toDTO :: Organization -> OrganizationDTO
toDTO organization =
  OrganizationDTO
  { _organizationDTOOrganizationId = organization ^. organizationId
  , _organizationDTOName = organization ^. name
  , _organizationDTODescription = organization ^. description
  , _organizationDTOEmail = organization ^. email
  , _organizationDTORole = organization ^. role
  , _organizationDTOToken = organization ^. token
  , _organizationDTOActive = organization ^. active
  , _organizationDTOCreatedAt = organization ^. createdAt
  , _organizationDTOUpdatedAt = organization ^. updatedAt
  , _organizationDTOLastAccessAt = organization ^. lastAccessAt
  }

toSimpleDTO :: Organization -> OrganizationSimpleDTO
toSimpleDTO organization =
  OrganizationSimpleDTO
  { _organizationSimpleDTOOrganizationId = organization ^. organizationId
  , _organizationSimpleDTOName = organization ^. name
  }

organizationDTOtoSimpleDTO :: OrganizationDTO -> OrganizationSimpleDTO
organizationDTOtoSimpleDTO organization =
  OrganizationSimpleDTO
  { _organizationSimpleDTOOrganizationId = organization ^. organizationId
  , _organizationSimpleDTOName = organization ^. name
  }

fromCreateDTO :: OrganizationCreateDTO -> OrganizationRole -> String -> UTCTime -> UTCTime -> UTCTime -> Organization
fromCreateDTO dto orgRole orgToken orgCreatedAt orgUpdatedAt orgLastAccessAt =
  Organization
  { _organizationOrganizationId = dto ^. organizationId
  , _organizationName = dto ^. name
  , _organizationDescription = dto ^. description
  , _organizationEmail = dto ^. email
  , _organizationRole = orgRole
  , _organizationToken = orgToken
  , _organizationActive = False
  , _organizationCreatedAt = orgCreatedAt
  , _organizationUpdatedAt = orgUpdatedAt
  , _organizationLastAccessAt = orgLastAccessAt
  }

fromChangeDTO :: OrganizationChangeDTO -> OrganizationDTO -> UTCTime -> Organization
fromChangeDTO dto org orgUpdatedAt =
  Organization
  { _organizationOrganizationId = org ^. organizationId
  , _organizationName = dto ^. name
  , _organizationDescription = dto ^. description
  , _organizationEmail = dto ^. email
  , _organizationRole = org ^. role
  , _organizationToken = org ^. token
  , _organizationActive = org ^. active
  , _organizationCreatedAt = org ^. createdAt
  , _organizationUpdatedAt = orgUpdatedAt
  , _organizationLastAccessAt = org ^. lastAccessAt
  }
