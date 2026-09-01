module Specs.Api.Handler.Locale.Detail_Bundle_GET (
  detail_bundle_GET,
) where

import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import qualified RegistryServer.Database.Migration.Development.Locale.LocaleMigration as LOC_Migration
import RegistryServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- GET /locales/{lclId}/bundle
-- ------------------------------------------------------------------------
detail_bundle_GET :: RequestContext -> SpecWith ((), Application)
detail_bundle_GET requestContext =
  describe "GET /locales/{lclId}/bundle" $ do
    test_200 requestContext
    test_401 requestContext
    test_404 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/locales/global:dutch:1.0.0/bundle"

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
      runInContextIO LOC_Migration.runMigration requestContext
      runInContextIO LOC_Migration.runS3Migration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resDto) = destructResponse response :: (Int, ResponseHeaders, String)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders

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
    "/locales/global:non-existing-locale:1.0.0/bundle"
    reqHeaders
    reqBody
    "locale"
    [("organization_id", "global"), ("locale_id", "non-existing-locale"), ("version", "1.0.0")]
