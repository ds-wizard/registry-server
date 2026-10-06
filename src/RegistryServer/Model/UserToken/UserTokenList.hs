module RegistryServer.Model.UserToken.UserTokenList where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data UserTokenList = UserTokenList
  { uuid :: U.UUID
  , name :: String
  , expiresAt :: Maybe UTCTime
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
