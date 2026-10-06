module Specs.Api.Handler.ApiKey.Detail_DELETE (
  detail_DELETE,
) where

import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai

import RegistryServer.Database.DAO.UserToken.UserTokenDAO
import RegistryServer.Database.Migration.Development.User.Data.Users
import RegistryServer.Model.Context.RequestContext
import RegistryServer.Model.User.User
import RegistryServer.Model.UserToken.UserToken

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common

-- ------------------------------------------------------------------------
-- DELETE /api/api-keys/{uuid}
-- ------------------------------------------------------------------------
detail_DELETE :: RequestContext -> SpecWith ((), Application)
detail_DELETE requestContext =
  describe "DELETE /api/api-keys/{uuid}" $ do
    test_204 requestContext
    test_401 requestContext
    test_404 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodDelete

reqUrl = "/api/api-keys/9d3b1e7c-2f5a-4c8d-b6e0-1a4f7c9e2b35"

reqHeaders = [reqUserAuthHeader]

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
      assertCountInDB (findUserTokensByUserUuidAndType userNikola.uuid ApiKeyUserTokenType) requestContext 0

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 requestContext =
  createNotFoundTest
    reqMethod
    "/api/api-keys/0c5e8f2a-6b4d-4e1f-9a3c-7d2b5e8f1a04"
    reqHeaders
    reqBody
    "user_token"
    [("uuid", "0c5e8f2a-6b4d-4e1f-9a3c-7d2b5e8f1a04"), ("user_uuid", "7b1f0d3e-8a2c-4f6b-b5d9-2c4e6a8f0b13")]
