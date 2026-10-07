module RegistryServer.Model.MailTemplateLocale.RegistryMailTemplateLocale where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data RegistryMailTemplateLocale = RegistryMailTemplateLocale
  { uuid :: U.UUID
  , code :: String
  , name :: String
  , version :: String
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
