module Api.Resource.Organization.OrganizationSimpleJM where

import Data.Aeson

import Api.Resource.Organization.OrganizationSimpleDTO
import Util.JSON (simpleParseJSON)

instance FromJSON OrganizationSimpleDTO where
  parseJSON = simpleParseJSON "_organizationSimpleDTO"

instance ToJSON OrganizationSimpleDTO where
  toJSON OrganizationSimpleDTO {..} =
    object
      [ "organizationId" .= _organizationSimpleDTOOrganizationId
      , "name" .= _organizationSimpleDTOName
      , "logo" .= _organizationSimpleDTOLogo
      ]
