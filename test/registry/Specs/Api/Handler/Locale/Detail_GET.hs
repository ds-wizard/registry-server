module Specs.Api.Handler.Locale.Detail_GET (
  detail_GET,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import RegistryServer.Api.Resource.Locale.LocaleDetailJM ()
import qualified RegistryServer.Database.Migration.Development.Locale.LocaleMigration as TML_Migration
import RegistryServer.Model.Context.RequestContext
import RegistryServer.Service.Locale.LocaleMapper
import Shared.Database.Migration.Development.Locale.Data.Locales
import Shared.Model.Locale.Locale

import SharedTest.Specs.Api.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- GET /api/locales/{lclId}
-- ------------------------------------------------------------------------
detail_GET :: RequestContext -> SpecWith ((), Application)
detail_GET requestContext =
  describe "GET /api/locales/{lclId}" $ do
    test_200 requestContext
    test_404 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/api/locales/global.dutch:1.0.0"

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
      let expDto = toDetailDTO localeNl [localeNl.version]
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
    "/api/locales/global.non-existing-locale:1.0.0"
    reqHeaders
    reqBody
    "locale"
    [("id", "global.non-existing-locale"), ("version", "1.0.0")]
