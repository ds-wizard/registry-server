module RegistryServer.Api.Resource.Locale.LocaleDetailJM where

import Data.Aeson

import RegistryPublic.Api.Resource.Organization.OrganizationSimpleJM ()
import RegistryServer.Api.Resource.Locale.LocaleDetailDTO
import Shared.Util.Aeson

instance FromJSON LocaleDetailDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON LocaleDetailDTO where
  toJSON = genericToJSON jsonOptions
