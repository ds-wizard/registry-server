module Specs.Api.Handler.ApiKey.List_GET (
  list_GET,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import RegistryServer.Api.Resource.UserToken.UserTokenListJM ()
import RegistryServer.Database.DAO.User.UserDAO
import RegistryServer.Database.Migration.Development.User.Data.Users
import RegistryServer.Database.Migration.Development.UserToken.Data.UserTokens
import RegistryServer.Localization.Messages.Public
import RegistryServer.Model.Context.RequestContext
import RegistryServer.Model.User.User
import RegistryServer.Model.UserToken.UserTokenList
import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Model.Error.Error

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- GET /api/api-keys
-- ------------------------------------------------------------------------
list_GET :: RequestContext -> SpecWith ((), Application)
list_GET requestContext =
  describe "GET /api/api-keys" $ do
    test_200 requestContext
    test_401 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/api/api-keys"

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
      let expDto = [adminApiKeyList]
      let expType (a :: [UserTokenList]) = a
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      assertListResponse expStatus expHeaders expDto expType response

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = do
  createAuthTest reqMethod reqUrl [reqCtHeader] reqBody
  it "HTTP 401 UNAUTHORIZED when account is not active" $ do
    -- GIVEN: Prepare expectation
    let expStatus = 401
    let expHeaders = resCtHeader : resCorsHeaders
    let expBody = encode . UnauthorizedError $ _ERROR_SERVICE_TOKEN__ACCOUNT_IS_NOT_ACTIVATED
    -- AND: Prepare DB
    runInContextIO (updateUserByUuid (userNikola {active = False})) requestContext
    -- WHEN: Call API
    response <- request reqMethod reqUrl [reqUserAuthHeader] reqBody
    -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher
