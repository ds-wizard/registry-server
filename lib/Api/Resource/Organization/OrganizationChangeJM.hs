module Api.Resource.Organization.OrganizationChangeJM where

import Data.Aeson

import Api.Resource.Organization.OrganizationChangeDTO
import Util.JSON (simpleParseJSON)

instance FromJSON OrganizationChangeDTO where
  parseJSON = simpleParseJSON "_organizationChangeDTO"

instance ToJSON OrganizationChangeDTO where
  toJSON OrganizationChangeDTO {..} =
    object
      [ "name" .= _organizationChangeDTOName
      , "description" .= _organizationChangeDTODescription
      , "email" .= _organizationChangeDTOEmail
      ]
