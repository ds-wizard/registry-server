module RegistryServer.Api.Resource.User.UserStateJM where

import Data.Aeson

import RegistryServer.Api.Resource.User.UserStateDTO
import Shared.Util.Aeson

instance ToJSON UserStateDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON UserStateDTO where
  parseJSON = genericParseJSON jsonOptions
