module Specs.Api.Handler.Locale.Detail_Po_GET (
  detail_po_GET,
) where

import qualified Data.ByteString.Lazy.Char8 as BSL
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import RegistryServer.Database.Migration.Development.Locale.Data.RegistryLocales
import qualified RegistryServer.Database.Migration.Development.Locale.LocaleMigration as LOC_Migration
import RegistryServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- GET /api/locales/{code}/{version}/po
-- ------------------------------------------------------------------------
detail_po_GET :: RequestContext -> SpecWith ((), Application)
detail_po_GET requestContext =
  describe "GET /api/locales/{code}/{version}/po" $ do
    test_200 requestContext
    test_404 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/api/locales/de/5.0.0/po"

reqHeaders = []

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext =
  it "HTTP 200 OK" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = ["Content-Type" <:> "text/x-gettext-translation"]
      let expBody = BSL.fromStrict registryLocaleDePo
      -- AND: Run migrations
      runInContextIO LOC_Migration.runMigration requestContext
      runInContextIO LOC_Migration.runS3Migration requestContext
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
  createNotFoundTest
    reqMethod
    "/api/locales/de/9.9.9/po"
    reqHeaders
    reqBody
    "locale"
    [("code", "de"), ("version", "9.9.9")]
