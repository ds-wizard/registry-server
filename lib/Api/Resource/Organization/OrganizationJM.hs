module Api.Resource.Organization.OrganizationJM where

import Data.Aeson

import Api.Resource.Organization.Common ()
import Api.Resource.Organization.OrganizationDTO
import Util.JSON (simpleParseJSON, simpleToJSON)

instance ToJSON OrganizationDTO where
  toJSON = simpleToJSON "_organizationDTO"

instance FromJSON OrganizationDTO where
  parseJSON = simpleParseJSON "_organizationDTO"
