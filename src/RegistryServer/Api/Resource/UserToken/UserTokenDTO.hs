module RegistryServer.Api.Resource.UserToken.UserTokenDTO where

import Data.Time
import GHC.Generics

data UserTokenDTO = UserTokenDTO
  { token :: String
  , expiresAt :: Maybe UTCTime
  }
  deriving (Show, Eq, Generic)
