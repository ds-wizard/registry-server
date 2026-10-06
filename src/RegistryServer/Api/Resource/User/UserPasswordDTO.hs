module RegistryServer.Api.Resource.User.UserPasswordDTO where

import GHC.Generics

data UserPasswordDTO = UserPasswordDTO
  { password :: String
  }
  deriving (Show, Eq, Generic)
