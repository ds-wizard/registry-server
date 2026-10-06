module Specs.Api.Handler.User.List_POST (
  list_POST,
) where

import Data.Aeson (eitherDecode, encode)
import qualified Data.Map.Strict as M
import Network.HTTP.Types
import Network.Wai (Application)
import Network.Wai.Test hiding (request)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import RegistryServer.Api.Resource.User.UserCreateDTO
import RegistryServer.Api.Resource.User.UserCreateJM ()
import RegistryServer.Api.Resource.User.UserDTO
import RegistryServer.Api.Resource.User.UserJM ()
import RegistryServer.Database.DAO.User.UserDAO
import RegistryServer.Database.Mapping.UserEmailLink.UserEmailLinkType ()
import RegistryServer.Database.Migration.Development.User.Data.Users
import RegistryServer.Localization.Messages.Public
import RegistryServer.Model.Context.RequestContext
import RegistryServer.Model.User.User
import RegistryServer.Model.UserEmailLink.UserEmailLinkType
import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Database.DAO.UserEmailLink.UserEmailLinkDAO
import Shared.Model.Error.Error
import Shared.Model.UserEmailLink.UserEmailLink
import Shared.Util.Password

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common

-- ------------------------------------------------------------------------
-- POST /api/users
-- ------------------------------------------------------------------------
list_POST :: RequestContext -> SpecWith ((), Application)
list_POST requestContext =
  describe "POST /api/users" $ do
    test_201 requestContext
    test_201_by_admin requestContext
    test_201_existing_email requestContext
    test_400 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/users"

reqHeaders = [reqCtHeader]

reqDto = userIsaacCreate

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
    let (Right resDto) = eitherDecode body :: Either String UserDTO
    liftIO $ resDto.email `shouldBe` reqDto.email
    liftIO $ resDto.firstName `shouldBe` reqDto.firstName
    liftIO $ resDto.lastName `shouldBe` reqDto.lastName
    liftIO $ resDto.role `shouldBe` UserRole
    liftIO $ resDto.active `shouldBe` False
    -- AND: Find result in DB and compare with expectation state
    userFromDb <- getOneFromDB (findUserByEmail reqDto.email) requestContext
    liftIO $ verifyPassword reqDto.password userFromDb.passwordHash `shouldBe` True
    assertCountInDB (findUserEmailLinks :: RequestContextM [UserEmailLink String UserEmailLinkType]) requestContext 1

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_201_by_admin requestContext =
  it "HTTP 201 CREATED (by admin - active, no confirmation needed)" $ do
    -- WHEN: Call API
    response <- request reqMethod reqUrl [reqAdminAuthHeader, reqCtHeader] reqBody
    -- THEN: Compare response with expectation
    let (SResponse (Status status _) _ body) = response
    liftIO $ status `shouldBe` 201
    let (Right resDto) = eitherDecode body :: Either String UserDTO
    liftIO $ resDto.active `shouldBe` True
    -- AND: Find result in DB and compare with expectation state
    userFromDb <- getOneFromDB (findUserByEmail reqDto.email) requestContext
    liftIO $ userFromDb.active `shouldBe` True

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_201_existing_email requestContext =
  it "HTTP 201 CREATED when email is already used (no account is created)" $ do
    -- GIVEN: Prepare request
    let reqDto = userIsaacCreate {email = userAdmin.email} :: UserCreateDTO
    let reqBody = encode reqDto
    -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- THEN: Compare response with expectation
    let (SResponse (Status status _) _ body) = response
    liftIO $ status `shouldBe` 201
    let (Right resDto) = eitherDecode body :: Either String UserDTO
    liftIO $ resDto.email `shouldBe` userAdmin.email
    liftIO $ resDto.active `shouldBe` False
    -- AND: Find result in DB and compare with expectation state
    assertCountInDB findUsers requestContext 2
    assertCountInDB (findUserEmailLinks :: RequestContextM [UserEmailLink String UserEmailLinkType]) requestContext 0

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 requestContext = do
  createInvalidJsonTest reqMethod reqUrl "email"
  it "HTTP 400 BAD REQUEST when an admin uses an email that is already used" $
    -- GIVEN: Prepare request
    do
      let reqDto = userIsaacCreate {email = userAdmin.email} :: UserCreateDTO
      let reqBody = encode reqDto
      -- AND: Prepare expectation
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = ValidationError [] (M.singleton "email" [_ERROR_VALIDATION__USER_EMAIL_UNIQUENESS userAdmin.email])
      let expBody = encode expDto
      -- WHEN: Call API
      response <- request reqMethod reqUrl [reqAdminAuthHeader, reqCtHeader] reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
