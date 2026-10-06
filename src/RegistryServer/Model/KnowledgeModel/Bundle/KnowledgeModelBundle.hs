module RegistryServer.Model.KnowledgeModel.Bundle.KnowledgeModelBundle where

import GHC.Generics

import RegistryServer.Model.KnowledgeModel.Package.KnowledgeModelPackageRaw

data KnowledgeModelBundle = KnowledgeModelBundle
  { id :: String
  , name :: String
  , version :: String
  , metamodelVersion :: Int
  , packages :: [KnowledgeModelPackageRaw]
  }
  deriving (Show, Eq, Generic)
