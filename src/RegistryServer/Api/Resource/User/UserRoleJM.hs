module RegistryServer.Api.Resource.User.UserRoleJM where

import Data.Aeson

import RegistryServer.Model.User.User

instance ToJSON UserRole

instance FromJSON UserRole
