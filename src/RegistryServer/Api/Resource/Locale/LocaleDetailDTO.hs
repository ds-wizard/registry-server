module RegistryServer.Api.Resource.Locale.LocaleDetailDTO where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data LocaleDetailDTO = LocaleDetailDTO
  { uuid :: U.UUID
  , name :: String
  , description :: String
  , code :: String
  , id :: String
  , version :: String
  , license :: String
  , readme :: String
  , recommendedAppVersion :: String
  , versions :: [String]
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
