module RegistryServer.Api.Resource.Organization.OrganizationChangeJM where

import Data.Aeson

import RegistryServer.Api.Resource.Organization.OrganizationChangeDTO
import Shared.Util.Aeson

instance ToJSON OrganizationChangeDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON OrganizationChangeDTO where
  parseJSON = genericParseJSON jsonOptions
