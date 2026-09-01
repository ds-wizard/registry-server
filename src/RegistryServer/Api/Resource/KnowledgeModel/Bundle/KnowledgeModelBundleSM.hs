module RegistryServer.Api.Resource.KnowledgeModel.Bundle.KnowledgeModelBundleSM where

import Data.Swagger

import RegistryServer.Api.Resource.KnowledgeModel.Bundle.KnowledgeModelBundleJM ()
import RegistryServer.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageRawSM ()
import RegistryServer.Model.KnowledgeModel.Bundle.KnowledgeModelBundle

instance ToSchema KnowledgeModelBundle where
  declareNamedSchema _ = return $ NamedSchema (Just "KnowledgeModelBundle") binarySchema
