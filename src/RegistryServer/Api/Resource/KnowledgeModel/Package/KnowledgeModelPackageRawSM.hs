module RegistryServer.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageRawSM where

import Data.Swagger

import RegistryServer.Model.KnowledgeModel.Package.KnowledgeModelPackageRaw

instance ToSchema KnowledgeModelPackageRaw where
  declareNamedSchema _ = return $ NamedSchema (Just "KnowledgeModelPackageRaw") binarySchema
