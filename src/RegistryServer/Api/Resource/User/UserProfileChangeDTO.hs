module RegistryServer.Api.Resource.User.UserProfileChangeDTO where

import GHC.Generics

data UserProfileChangeDTO = UserProfileChangeDTO
  { firstName :: String
  , lastName :: String
  , email :: String
  }
  deriving (Show, Eq, Generic)
