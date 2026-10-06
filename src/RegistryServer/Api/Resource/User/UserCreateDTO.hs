module RegistryServer.Api.Resource.User.UserCreateDTO where

import GHC.Generics

data UserCreateDTO = UserCreateDTO
  { email :: String
  , firstName :: String
  , lastName :: String
  , password :: String
  }
  deriving (Show, Eq, Generic)
