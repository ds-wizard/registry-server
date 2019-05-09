module Api.Resource.Organization.OrganizationSimpleDTO where

import GHC.Generics

data OrganizationSimpleDTO = OrganizationSimpleDTO
  { _organizationSimpleDTOOrganizationId :: String
  , _organizationSimpleDTOName :: String
  } deriving (Show, Eq, Generic)
