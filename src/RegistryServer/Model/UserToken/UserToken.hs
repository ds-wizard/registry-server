module RegistryServer.Model.UserToken.UserToken where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data UserTokenType
  = LoginUserTokenType
  | ApiKeyUserTokenType
  deriving (Show, Eq, Generic, Read)

data UserToken = UserToken
  { uuid :: U.UUID
  , name :: String
  , tType :: UserTokenType
  , userUuid :: U.UUID
  , valueHash :: String
  , expiresAt :: Maybe UTCTime
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
