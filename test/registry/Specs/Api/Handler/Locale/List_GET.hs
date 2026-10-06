module Specs.Api.Handler.Locale.List_GET (
  list_GET,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import RegistryPublic.Api.Resource.Locale.LocaleJM ()
import qualified RegistryServer.Database.Migration.Development.Locale.LocaleMigration as TML_Migration
import RegistryServer.Model.Context.RequestContext
import RegistryServer.Service.Locale.LocaleMapper
import Shared.Database.Migration.Development.Locale.Data.Locales

import SharedTest.Specs.Api.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- GET /api/locales
-- ------------------------------------------------------------------------
list_GET :: RequestContext -> SpecWith ((), Application)
list_GET requestContext = describe "GET /api/locales" $ test_200 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/api/locales"

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
      let expDto = [toDTO localeNl]
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO TML_Migration.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
