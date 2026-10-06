module Specs.Api.Handler.User.List_GET (
  list_GET,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import RegistryServer.Api.Resource.User.UserJM ()
import RegistryServer.Database.Migration.Development.User.Data.Users
import RegistryServer.Model.Context.RequestContext
import Shared.Api.Resource.Common.PageJM ()
import Shared.Model.Common.Page
import Shared.Model.Common.PageMetadata

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common

-- ------------------------------------------------------------------------
-- GET /api/users
-- ------------------------------------------------------------------------
list_GET :: RequestContext -> SpecWith ((), Application)
list_GET requestContext =
  describe "GET /api/users" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/api/users"

reqHeaders = [reqAdminAuthHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  create_test_200
    "HTTP 200 OK"
    "/api/users?sort=email,asc"
    (Page "users" (PageMetadata 20 2 1 0) [userAdminDTO, userNikolaDTO])
  create_test_200
    "HTTP 200 OK (pagination)"
    "/api/users?sort=email,asc&page=1&size=1"
    (Page "users" (PageMetadata 1 2 2 1) [userNikolaDTO])
  create_test_200
    "HTTP 200 OK (query)"
    "/api/users?sort=email,asc&q=tesla"
    (Page "users" (PageMetadata 20 1 1 0) [userNikolaDTO])
  create_test_200
    "HTTP 200 OK (role)"
    "/api/users?sort=email,asc&role=AdminRole"
    (Page "users" (PageMetadata 20 1 1 0) [userAdminDTO])

create_test_200 title reqUrl expDto =
  it title $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expBody = encode expDto
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createForbiddenTest reqMethod reqUrl [reqUserAuthHeader] reqBody "List Users"
