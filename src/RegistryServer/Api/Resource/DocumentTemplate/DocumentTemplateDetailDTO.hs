module RegistryServer.Api.Resource.DocumentTemplate.DocumentTemplateDetailDTO where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.Common.SemVer2Tuple

data DocumentTemplateDetailDTO = DocumentTemplateDetailDTO
  { uuid :: U.UUID
  , name :: String
  , id :: String
  , version :: String
  , metamodelVersion :: SemVer2Tuple
  , description :: String
  , readme :: String
  , license :: String
  , versions :: [String]
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
