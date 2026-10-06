module RegistryServer.Api.Resource.UserToken.UserTokenListJM where

import Data.Aeson

import RegistryServer.Model.UserToken.UserTokenList
import Shared.Util.Aeson

instance ToJSON UserTokenList where
  toJSON = genericToJSON jsonOptions

instance FromJSON UserTokenList where
  parseJSON = genericParseJSON jsonOptions
