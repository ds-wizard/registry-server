module RegistryServer.Api.Resource.Locale.LocaleJM where

import Data.Aeson

import RegistryPublic.Api.Resource.Organization.OrganizationSimpleJM ()
import RegistryServer.Api.Resource.Locale.LocaleDTO
import Shared.Util.Aeson

instance FromJSON LocaleDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON LocaleDTO where
  toJSON = genericToJSON jsonOptions
