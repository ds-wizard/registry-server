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
import RegistryServer.Database.Migration.Development.Locale.Data.RegistryLocales
import RegistryServer.Model.Context.RequestContext
import RegistryServer.Service.Locale.RegistryLocaleMapper

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Locale.Common

-- ------------------------------------------------------------------------
-- GET /api/locales
-- ------------------------------------------------------------------------
list_GET :: RequestContext -> SpecWith ((), Application)
list_GET requestContext =
  describe "GET /api/locales" $ do
    test_200 requestContext
    test_200_app_version requestContext
    test_200_unparsable_versions requestContext

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
  it "HTTP 200 OK (per code the newest version without appVersion)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = fmap toDTO [registryLocaleCs600, registryLocaleDe510, registryLocaleNl4100]
      let expBody = encode expDto
      -- AND: Run migrations
      runLocaleVersionsMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200_app_version requestContext =
  it "HTTP 200 OK (per code the newest version not newer than appVersion, else the oldest)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = fmap toDTO [registryLocaleCs520, registryLocaleDe, registryLocaleNl4100]
      let expBody = encode expDto
      -- AND: Run migrations
      runLocaleVersionsMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod "/api/locales?appVersion=5.0.3" reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200_unparsable_versions requestContext =
  it "HTTP 200 OK (unparsable versions sort last)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = fmap toDTO [registryLocaleCs520, registryLocaleDe400, registryLocaleNl490]
      let expBody = encode expDto
      -- AND: Run migrations
      runLocaleVersionsMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod "/api/locales?appVersion=1.0.0" reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
