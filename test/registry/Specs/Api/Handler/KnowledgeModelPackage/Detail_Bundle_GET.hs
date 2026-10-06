module Specs.Api.Handler.KnowledgeModelPackage.Detail_Bundle_GET (
  detail_bundle_GET,
) where

import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import RegistryServer.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageDetailJM ()
import RegistryServer.Database.Migration.Development.Audit.Data.AuditEntries
import RegistryServer.Model.Context.RequestContext
import Shared.Api.Resource.KnowledgeModel.Bundle.KnowledgeModelBundleJM ()
import Shared.Database.Migration.Development.KnowledgeModel.Data.Bundle.KnowledgeModelBundles
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.Model.Coordinate.Coordinate
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage ()

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Audit.Common
import Specs.Api.Handler.Common

-- ------------------------------------------------------------------------
-- GET /api/knowledge-model-packages/{pkgId}/bundle
-- ------------------------------------------------------------------------
detail_bundle_GET :: RequestContext -> SpecWith ((), Application)
detail_bundle_GET requestContext =
  describe "GET /api/knowledge-model-packages/{pkgId}/bundle" $ do
    test_200 requestContext
    test_401 requestContext
    test_404 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = BS.pack $ "/api/knowledge-model-packages/" ++ show (createCoordinate netherlandsKmPackageV2) ++ "/bundle"

reqHeaders = [reqAdminAuthHeader, reqCtHeader]

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
      let expDto = netherlandsV2KmBundle
      let expBody = encode expDto
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find result in DB and compare with expectation state
      assertExistenceOfAuditEntryInDB requestContext getKnowledgeModelBundleAuditEntry

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 requestContext =
  createNotFoundTest'
    reqMethod
    "/api/knowledge-model-packages/global.non-existing-km-package:1.0.0/bundle"
    reqHeaders
    reqBody
    "knowledge_model_package"
    [("id", "global.non-existing-km-package"), ("version", "1.0.0")]
