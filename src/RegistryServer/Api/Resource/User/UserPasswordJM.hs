module RegistryServer.Api.Resource.User.UserPasswordJM where

import Data.Aeson

import RegistryServer.Api.Resource.User.UserPasswordDTO
import Shared.Util.Aeson

instance ToJSON UserPasswordDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON UserPasswordDTO where
  parseJSON = genericParseJSON jsonOptions
