module Specs.Api.Handler.DocumentTemplate.Detail_GET (
  detail_GET,
) where

import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import RegistryServer.Api.Resource.DocumentTemplate.DocumentTemplateDetailJM ()
import RegistryServer.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import qualified RegistryServer.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML_Migration
import RegistryServer.Model.Context.RequestContext
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import Shared.Model.Coordinate.Coordinate

import SharedTest.Specs.Api.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- GET /api/document-templates/{documentTemplateId}
-- ------------------------------------------------------------------------
detail_GET :: RequestContext -> SpecWith ((), Application)
detail_GET requestContext =
  describe "GET /api/document-templates/{documentTemplateId}" $ do
    test_200 requestContext
    test_404 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = BS.pack $ "/api/document-templates/" ++ show (createCoordinate wizardDocumentTemplate)

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
      let expDto = wizardDocumentTemplateDetailDTO
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO TML_Migration.runMigration requestContext
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
    "/api/document-templates/global.non-existing-dt:1.0.0"
    reqHeaders
    reqBody
    "document_template"
    [("id", "global.non-existing-dt"), ("version", "1.0.0")]
