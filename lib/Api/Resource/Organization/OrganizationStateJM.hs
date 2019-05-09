module Api.Resource.Organization.OrganizationStateJM where

import Data.Aeson

import Api.Resource.Organization.OrganizationStateDTO
import Util.JSON (simpleParseJSON)

instance FromJSON OrganizationStateDTO where
  parseJSON = simpleParseJSON "_organizationStateDTO"

instance ToJSON OrganizationStateDTO where
  toJSON OrganizationStateDTO {..} = object ["active" .= _organizationStateDTOActive]
