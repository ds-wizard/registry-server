module RegistryServer.Api.Resource.User.UserStateDTO where

import GHC.Generics

data UserStateDTO = UserStateDTO
  { active :: Bool
  }
  deriving (Show, Eq, Generic)
