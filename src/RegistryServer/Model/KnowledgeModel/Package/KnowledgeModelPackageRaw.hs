module RegistryServer.Model.KnowledgeModel.Package.KnowledgeModelPackageRaw where

import Data.Aeson
import Data.Time
import GHC.Generics

import Shared.Model.Coordinate.Coordinate
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage

data KnowledgeModelPackageRaw = KnowledgeModelPackageRaw
  { id :: String
  , name :: String
  , version :: String
  , phase :: KnowledgeModelPackagePhase
  , metamodelVersion :: Int
  , description :: String
  , readme :: String
  , license :: String
  , language :: String
  , previousPackageId :: Maybe Coordinate
  , forkOfPackageId :: Maybe Coordinate
  , mergeCheckpointPackageId :: Maybe Coordinate
  , events :: Value
  , nonEditable :: Bool
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
