module Specs.Api.Handler.KnowledgeModelPackage.Detail_GET (
  detail_GET,
) where

import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import RegistryPublic.Database.Migration.Development.Organization.Data.Organizations
import RegistryServer.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageDetailJM ()
import RegistryServer.Model.Context.RequestContext
import RegistryServer.Service.KnowledgeModel.Package.KnowledgeModelPackageMapper
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.Model.Coordinate.Coordinate
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage ()

import SharedTest.Specs.Api.Common

-- ------------------------------------------------------------------------
-- GET /knowledge-model-packages/{pkgId}
-- ------------------------------------------------------------------------
detail_GET :: RequestContext -> SpecWith ((), Application)
detail_GET requestContext =
  describe "GET /knowledge-model-packages/{pkgId}" $ do
    test_200 requestContext
    test_404 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = BS.pack $ "/knowledge-model-packages/" ++ show (createCoordinate netherlandsKmPackageV2)

reqHeaders = [reqCtHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext =
  it "HTTP 200 OK" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toDetailDTO netherlandsKmPackageV2 ["1.0.0", "2.0.0"] orgNetherlands
      let expBody = encode expDto
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 requestContext =
  createNotFoundTest'
    reqMethod
    "/knowledge-model-packages/global:non-existing-km-package:1.0.0"
    reqHeaders
    reqBody
    "knowledge_model_package"
    [("organization_id", "global"), ("km_id", "non-existing-km-package"), ("version", "1.0.0")]
