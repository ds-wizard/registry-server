module RegistryServer.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageDetailDTO where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage

data KnowledgeModelPackageDetailDTO = KnowledgeModelPackageDetailDTO
  { uuid :: U.UUID
  , name :: String
  , id :: String
  , version :: String
  , phase :: KnowledgeModelPackagePhase
  , description :: String
  , readme :: String
  , license :: String
  , language :: String
  , metamodelVersion :: Int
  , previousPackageUuid :: Maybe U.UUID
  , forkOfPackageId :: Maybe String
  , forkOfPackageVersion :: Maybe String
  , mergeCheckpointPackageId :: Maybe String
  , mergeCheckpointPackageVersion :: Maybe String
  , versions :: [String]
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
