module RegistryServer.Api.Resource.UserToken.ApiKeyCreateJM where

import Data.Aeson

import RegistryServer.Api.Resource.UserToken.ApiKeyCreateDTO
import Shared.Util.Aeson

instance ToJSON ApiKeyCreateDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON ApiKeyCreateDTO where
  parseJSON = genericParseJSON jsonOptions
