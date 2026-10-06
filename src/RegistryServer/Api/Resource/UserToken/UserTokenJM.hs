module RegistryServer.Api.Resource.UserToken.UserTokenJM where

import Data.Aeson

import RegistryServer.Api.Resource.UserToken.UserTokenDTO
import Shared.Util.Aeson

instance ToJSON UserTokenDTO where
  toJSON = genericToJSON (jsonOptionsWithTypeField "type")

instance FromJSON UserTokenDTO where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "type")
