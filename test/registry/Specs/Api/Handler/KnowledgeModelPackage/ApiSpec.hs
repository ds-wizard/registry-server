module Specs.Api.Handler.KnowledgeModelPackage.ApiSpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Specs.Api.Handler.Common
import Specs.Api.Handler.KnowledgeModelPackage.Detail_Bundle_GET
import Specs.Api.Handler.KnowledgeModelPackage.Detail_GET
import Specs.Api.Handler.KnowledgeModelPackage.List_Bundle_POST
import Specs.Api.Handler.KnowledgeModelPackage.List_GET

knowledgeModelPackageAPI serverContext requestContext =
  with (startWebApp serverContext requestContext) $
    describe "KNOWLEDGE MODEL PACKAGE API Spec" $ do
      list_GET requestContext
      detail_GET requestContext
      detail_bundle_GET requestContext
      list_bundle_POST requestContext
