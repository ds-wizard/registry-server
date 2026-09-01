module Specs.Api.Handler.DocumentTemplate.Detail_Bundle_GET (
  detail_bundle_GET,
) where

import qualified Data.ByteString.Char8 as BS
import Network.HTTP.Types
import Network.Wai (Application)
import Network.Wai.Test (SResponse (..))
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import qualified RegistryServer.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML_Migration
import RegistryServer.Model.Context.RequestContext
import Shared.Api.Resource.DocumentTemplateBundle.DocumentTemplateBundleDTO
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Service.DocumentTemplate.Bundle.DocumentTemplateBundleMapper

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- GET /document-templates/{documentTemplateId}/bundle
-- ------------------------------------------------------------------------
detail_bundle_GET :: RequestContext -> SpecWith ((), Application)
detail_bundle_GET requestContext =
  describe "GET /document-templates/{documentTemplateId}/bundle" $ do
    test_200 requestContext
    test_401 requestContext
    test_404 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = BS.pack $ "/document-templates/" ++ show wizardDocumentTemplateCoordinate ++ "/bundle"

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
      let expHeaders = resCorsHeadersPlain
      -- AND: Run migrations
      runInContextIO TML_Migration.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resDto) = destructResponse response :: (Int, ResponseHeaders, String)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      let eBundle = fromDocumentTemplateArchive (simpleBody response)
      liftIO $ fmap ((.language) . fst) eBundle `shouldBe` Right wizardDocumentTemplate.language

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
    "/document-templates/global:non-existing-template:1.0.0/bundle"
    reqHeaders
    reqBody
    "document_template"
    [("organization_id", "global"), ("template_id", "non-existing-template"), ("version", "1.0.0")]
