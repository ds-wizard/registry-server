module RegistryServer.Api.Resource.UserEmailLink.UserEmailLinkJM where

import Data.Aeson

import RegistryServer.Model.UserEmailLink.UserEmailLinkType

instance FromJSON UserEmailLinkType

instance ToJSON UserEmailLinkType
