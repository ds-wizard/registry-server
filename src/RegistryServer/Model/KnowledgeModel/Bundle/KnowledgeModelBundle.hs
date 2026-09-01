module RegistryServer.Model.KnowledgeModel.Bundle.KnowledgeModelBundle where

import GHC.Generics

import RegistryServer.Model.KnowledgeModel.Package.KnowledgeModelPackageRaw
import Shared.Model.Coordinate.Coordinate

data KnowledgeModelBundle = KnowledgeModelBundle
  { bundleId :: Coordinate
  , name :: String
  , organizationId :: String
  , kmId :: String
  , version :: String
  , metamodelVersion :: Int
  , packages :: [KnowledgeModelPackageRaw]
  }
  deriving (Show, Eq, Generic)
