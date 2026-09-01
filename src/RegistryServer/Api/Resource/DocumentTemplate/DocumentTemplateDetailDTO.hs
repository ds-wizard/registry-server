module RegistryServer.Api.Resource.DocumentTemplate.DocumentTemplateDetailDTO where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import RegistryPublic.Model.Organization.OrganizationSimple
import Shared.Model.Common.SemVer2Tuple

data DocumentTemplateDetailDTO = DocumentTemplateDetailDTO
  { uuid :: U.UUID
  , name :: String
  , organizationId :: String
  , templateId :: String
  , version :: String
  , metamodelVersion :: SemVer2Tuple
  , description :: String
  , readme :: String
  , license :: String
  , versions :: [String]
  , organization :: OrganizationSimple
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
