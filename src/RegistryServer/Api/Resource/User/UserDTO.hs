module RegistryServer.Api.Resource.User.UserDTO where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import RegistryServer.Model.User.User

data UserDTO = UserDTO
  { uuid :: U.UUID
  , email :: String
  , firstName :: String
  , lastName :: String
  , role :: UserRole
  , active :: Bool
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
