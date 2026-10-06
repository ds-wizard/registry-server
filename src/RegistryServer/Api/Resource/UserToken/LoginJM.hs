module RegistryServer.Api.Resource.UserToken.LoginJM where

import Data.Aeson

import RegistryServer.Api.Resource.UserToken.LoginDTO
import Shared.Util.Aeson

instance ToJSON LoginDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON LoginDTO where
  parseJSON = genericParseJSON jsonOptions
