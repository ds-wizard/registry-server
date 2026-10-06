module RegistryServer.Api.Resource.Bootstrap.BootstrapJM where

import Data.Aeson

import RegistryServer.Api.Resource.Bootstrap.BootstrapDTO
import Shared.Util.Aeson

instance FromJSON BootstrapDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON BootstrapDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON BootstrapAuthenticationDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON BootstrapAuthenticationDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON BootstrapLocaleDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON BootstrapLocaleDTO where
  toJSON = genericToJSON jsonOptions
