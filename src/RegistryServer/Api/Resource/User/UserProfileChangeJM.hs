module RegistryServer.Api.Resource.User.UserProfileChangeJM where

import Data.Aeson

import RegistryServer.Api.Resource.User.UserProfileChangeDTO
import Shared.Util.Aeson

instance ToJSON UserProfileChangeDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON UserProfileChangeDTO where
  parseJSON = genericParseJSON jsonOptions
