module Specs.Api.Handler.User.Detail_GET (
  detail_GET,
) where

import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai

import RegistryServer.Api.Resource.User.UserDTO
import RegistryServer.Api.Resource.User.UserJM ()
import RegistryServer.Database.Migration.Development.User.Data.Users
import RegistryServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common

-- ------------------------------------------------------------------------
-- GET /api/users/{uuid}
-- ------------------------------------------------------------------------
detail_GET :: RequestContext -> SpecWith ((), Application)
detail_GET requestContext =
  describe "GET /api/users/{uuid}" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext
    test_404 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/api/users/7b1f0d3e-8a2c-4f6b-b5d9-2c4e6a8f0b13"

reqHeaders = [reqAdminAuthHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext =
  it "HTTP 200 OK" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      let expDto = userNikolaDTO
      let expType (a :: UserDTO) = a
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      assertResponse expStatus expHeaders expDto expType response

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createForbiddenTest reqMethod reqUrl [reqUserAuthHeader] reqBody "Detail User"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 requestContext =
  createNotFoundTest
    reqMethod
    "/api/users/dc9fe65f-748b-47ec-b30c-d255bbac64a0"
    reqHeaders
    reqBody
    "user_entity"
    [("uuid", "dc9fe65f-748b-47ec-b30c-d255bbac64a0")]
