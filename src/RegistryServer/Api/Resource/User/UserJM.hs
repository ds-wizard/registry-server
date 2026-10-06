module RegistryServer.Api.Resource.User.UserJM where

import Data.Aeson

import RegistryServer.Api.Resource.User.UserDTO
import RegistryServer.Api.Resource.User.UserRoleJM ()
import Shared.Util.Aeson

instance ToJSON UserDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON UserDTO where
  parseJSON = genericParseJSON jsonOptions
