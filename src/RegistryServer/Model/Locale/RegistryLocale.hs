module RegistryServer.Model.Locale.RegistryLocale where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data RegistryLocale = RegistryLocale
  { uuid :: U.UUID
  , code :: String
  , name :: String
  , version :: String
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
