module Api.Resource.Organization.OrganizationCreateJM where

import Data.Aeson

import Api.Resource.Organization.OrganizationCreateDTO
import Util.JSON (simpleParseJSON)

instance FromJSON OrganizationCreateDTO where
  parseJSON = simpleParseJSON "_organizationCreateDTO"

instance ToJSON OrganizationCreateDTO where
  toJSON OrganizationCreateDTO {..} =
    object
      [ "organizationId" .= _organizationCreateDTOOrganizationId
      , "name" .= _organizationCreateDTOName
      , "description" .= _organizationCreateDTODescription
      , "email" .= _organizationCreateDTOEmail
      ]
