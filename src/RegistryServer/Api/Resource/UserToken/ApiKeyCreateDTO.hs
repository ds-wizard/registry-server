module RegistryServer.Api.Resource.UserToken.ApiKeyCreateDTO where

import GHC.Generics

data ApiKeyCreateDTO = ApiKeyCreateDTO
  { name :: String
  }
  deriving (Show, Eq, Generic)
