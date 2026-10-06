module Specs.Api.Handler.Token.List_POST (
  list_POST,
) where

import Control.Monad (void)
import Data.Aeson (eitherDecode, encode)
import qualified Data.ByteString.Char8 as BS
import Network.HTTP.Types
import Network.Wai (Application)
import Network.Wai.Test hiding (request)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import RegistryServer.Api.Resource.UserToken.LoginDTO
import RegistryServer.Api.Resource.UserToken.LoginJM ()
import RegistryServer.Api.Resource.UserToken.UserTokenDTO
import RegistryServer.Api.Resource.UserToken.UserTokenJM ()
import RegistryServer.Database.DAO.User.UserDAO
import RegistryServer.Database.Migration.Development.User.Data.Users
import RegistryServer.Database.Migration.Development.UserToken.Data.UserTokens
import RegistryServer.Localization.Messages.Public
import RegistryServer.Model.Context.RequestContext
import RegistryServer.Model.User.User
import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Model.Error.Error

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- POST /api/tokens
-- ------------------------------------------------------------------------
list_POST :: RequestContext -> SpecWith ((), Application)
list_POST requestContext =
  describe "POST /api/tokens" $ do
    test_201 requestContext
    test_400 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/tokens"

reqHeaders = [reqCtHeader]

reqDto = adminLoginDto

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_201 requestContext =
  it "HTTP 201 CREATED" $ do
    -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- THEN: Compare response with expectation
    let (SResponse (Status status _) _ body) = response
    liftIO $ status `shouldBe` 201
    let (Right resDto) = eitherDecode body :: Either String UserTokenDTO
    liftIO $ resDto.expiresAt `shouldNotBe` Nothing
    -- AND: The token authenticates
    let authHeader = ("Authorization", BS.pack $ "Bearer " ++ resDto.token)
    currentResponse <- request methodGet "/api/users/current" [authHeader] ""
    let (SResponse (Status currentStatus _) _ _) = currentResponse
    liftIO $ currentStatus `shouldBe` 200

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 requestContext = do
  createInvalidJsonTest reqMethod reqUrl "email"
  createLoginErrorTest "HTTP 400 BAD REQUEST when password is wrong" (reqDto {password = "wrong"}) _ERROR_SERVICE_TOKEN__INCORRECT_EMAIL_OR_PASSWORD (return ())
  createLoginErrorTest "HTTP 400 BAD REQUEST when email is unknown" (reqDto {email = "unknown@example.com"}) _ERROR_SERVICE_TOKEN__INCORRECT_EMAIL_OR_PASSWORD (return ())
  createLoginErrorTest
    "HTTP 400 BAD REQUEST when account is not active"
    reqDto
    _ERROR_SERVICE_TOKEN__ACCOUNT_IS_NOT_ACTIVATED
    (void $ runInContextIO (updateUserByUuid (userAdmin {active = False})) requestContext)

createLoginErrorTest name reqDto message prepareDb =
  it name $ do
    -- GIVEN: Prepare request
    let reqBody = encode (reqDto :: LoginDTO)
    -- AND: Prepare expectation
    let expStatus = 400
    let expHeaders = resCtHeader : resCorsHeaders
    let expBody = encode . UserError $ message
    -- AND: Prepare DB
    liftIO prepareDb
    -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher
