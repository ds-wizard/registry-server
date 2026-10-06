module Specs.Api.Handler.User.List_Current_GET (
  list_current_GET,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import RegistryServer.Api.Resource.User.UserDTO
import RegistryServer.Api.Resource.User.UserJM ()
import RegistryServer.Database.DAO.UserToken.UserTokenDAO
import RegistryServer.Database.Migration.Development.User.Data.Users
import RegistryServer.Database.Migration.Development.UserToken.Data.UserTokens
import RegistryServer.Localization.Messages.Public
import RegistryServer.Model.Context.RequestContext
import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Model.Error.Error

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- GET /api/users/current
-- ------------------------------------------------------------------------
list_current_GET :: RequestContext -> SpecWith ((), Application)
list_current_GET requestContext =
  describe "GET /api/users/current" $ do
    test_200 requestContext
    test_401 requestContext
    test_401_expired_token requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/api/users/current"

reqHeaders = [reqUserAuthHeader]

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
test_401_expired_token requestContext =
  it "HTTP 401 UNAUTHORIZED when the login token is expired" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 401
      let expHeaders = resCtHeader : resCorsHeaders
      let expBody = encode (UnauthorizedError _ERROR_API_COMMON__UNABLE_TO_GET_USER)
      -- AND: Prepare DB
      runInContextIO (insertUserToken nikolaExpiredLoginToken) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl [("Authorization", "Bearer ExpiredToken")] reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
