module RegistryServer.Api.Resource.KnowledgeModel.Bundle.KnowledgeModelBundleJM where

import Data.Aeson

import RegistryServer.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageRawJM ()
import RegistryServer.Model.KnowledgeModel.Bundle.KnowledgeModelBundle
import Shared.Util.Aeson

instance FromJSON KnowledgeModelBundle where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON KnowledgeModelBundle where
  toJSON = genericToJSON jsonOptions
