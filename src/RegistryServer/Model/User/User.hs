module RegistryServer.Model.User.User where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data UserRole
  = AdminRole
  | UserRole
  deriving (Show, Eq, Generic, Read)

data User = User
  { uuid :: U.UUID
  , email :: String
  , firstName :: String
  , lastName :: String
  , passwordHash :: String
  , role :: UserRole
  , active :: Bool
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Generic)

instance Eq User where
  a == b =
    a.uuid == b.uuid
      && a.email == b.email
      && a.firstName == b.firstName
      && a.lastName == b.lastName
      && a.passwordHash == b.passwordHash
      && a.role == b.role
      && a.active == b.active
