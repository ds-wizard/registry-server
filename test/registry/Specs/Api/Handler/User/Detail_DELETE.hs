module Specs.Api.Handler.User.Detail_DELETE (
  detail_DELETE,
) where

import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai

import RegistryServer.Database.DAO.User.UserDAO
import RegistryServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common

-- ------------------------------------------------------------------------
-- DELETE /api/users/{uuid}
-- ------------------------------------------------------------------------
detail_DELETE :: RequestContext -> SpecWith ((), Application)
detail_DELETE requestContext =
  describe "DELETE /api/users/{uuid}" $ do
    test_204 requestContext
    test_401 requestContext
    test_403 requestContext
    test_404 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodDelete

reqUrl = "/api/users/7b1f0d3e-8a2c-4f6b-b5d9-2c4e6a8f0b13"

reqHeaders = [reqAdminAuthHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_204 requestContext =
  it "HTTP 204 NO CONTENT" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 204
      let expHeaders = resCorsHeadersPlain
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      assertEmptyResponse expStatus expHeaders response
      -- AND: Find result in DB and compare with expectation state
      assertCountInDB findUsers requestContext 1

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createForbiddenTest reqMethod reqUrl [reqUserAuthHeader] reqBody "Delete User"

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
