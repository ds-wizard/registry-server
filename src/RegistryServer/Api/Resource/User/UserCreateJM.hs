module RegistryServer.Api.Resource.User.UserCreateJM where

import Data.Aeson

import RegistryServer.Api.Resource.User.UserCreateDTO
import Shared.Util.Aeson

instance ToJSON UserCreateDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON UserCreateDTO where
  parseJSON = genericParseJSON jsonOptions
