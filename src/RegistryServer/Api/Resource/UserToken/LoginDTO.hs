module RegistryServer.Api.Resource.UserToken.LoginDTO where

import GHC.Generics

data LoginDTO = LoginDTO
  { email :: String
  , password :: String
  }
  deriving (Show, Eq, Generic)
