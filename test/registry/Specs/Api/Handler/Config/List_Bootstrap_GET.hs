module Specs.Api.Handler.Config.List_Bootstrap_GET (
  list_bootstrap_GET,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import RegistryServer.Api.Resource.Config.ClientConfigJM ()
import RegistryServer.Model.Context.RequestContext
import RegistryServer.Service.Config.Client.ClientConfigMapper

import SharedTest.Specs.Api.Common

-- ------------------------------------------------------------------------
-- GET /configs/bootstrap
-- ------------------------------------------------------------------------
list_bootstrap_GET :: RequestContext -> SpecWith ((), Application)
list_bootstrap_GET requestContext = describe "GET /configs/bootstrap" $ test_200 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/configs/bootstrap"

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
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toClientConfigDTO requestContext.serverConfig
      let expBody = encode expDto
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
