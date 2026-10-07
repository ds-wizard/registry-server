module Specs.Api.Handler.MailTemplateLocale.List_GET (
  list_GET,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import RegistryPublic.Api.Resource.Locale.LocaleJM ()
import RegistryServer.Database.Migration.Development.MailTemplateLocale.Data.RegistryMailTemplateLocales
import qualified RegistryServer.Database.Migration.Development.MailTemplateLocale.MailTemplateLocaleMigration as MTL_Migration
import RegistryServer.Model.Context.RequestContext
import RegistryServer.Service.MailTemplateLocale.RegistryMailTemplateLocaleMapper

import SharedTest.Specs.Api.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- GET /api/mail-template-locales
-- ------------------------------------------------------------------------
list_GET :: RequestContext -> SpecWith ((), Application)
list_GET requestContext =
  describe "GET /api/mail-template-locales" $ do
    test_200 requestContext
    test_200_older_app_version requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/api/mail-template-locales?appVersion=5.0.3"

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
      let expDto = [toDTO registryMailTemplateLocaleDe]
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO MTL_Migration.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200_older_app_version requestContext =
  it "HTTP 200 OK (appVersion older than every version)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = [toDTO registryMailTemplateLocaleDe]
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO MTL_Migration.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod "/api/mail-template-locales?appVersion=4.0.0" reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
