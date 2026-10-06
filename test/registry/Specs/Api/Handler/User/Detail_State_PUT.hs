module Specs.Api.Handler.User.Detail_State_PUT (
  detail_state_PUT,
) where

import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import RegistryServer.Api.Resource.User.UserStateDTO
import RegistryServer.Api.Resource.User.UserStateJM ()
import RegistryServer.Database.DAO.User.UserDAO
import RegistryServer.Database.Mapping.UserEmailLink.UserEmailLinkType ()
import RegistryServer.Database.Migration.Development.User.Data.Users
import RegistryServer.Database.Migration.Development.UserEmailLink.Data.UserEmailLinks
import RegistryServer.Model.Context.RequestContext
import RegistryServer.Model.User.User
import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Constant.Tenant
import Shared.Database.DAO.UserEmailLink.UserEmailLinkDAO
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Shared.Model.UserEmailLink.UserEmailLink

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.User.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- PUT /api/users/{uuid}/state
-- ------------------------------------------------------------------------
detail_state_PUT :: RequestContext -> SpecWith ((), Application)
detail_state_PUT requestContext =
  describe "PUT /api/users/{uuid}/state" $ do
    test_200 requestContext
    test_400 requestContext
    test_404 requestContext
    test_404_forgotten_password_hash requestContext
    test_404_used_hash requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqUrl = reqUrlWithHash registrationUserEmailLink.hash

reqUrlWithHash hash = BS.pack $ "/api/users/4e2a6c9f-3d7b-4b8e-9c1a-5f0d2e8b7a61/state?hash=" ++ hash

reqHeaders = [reqCtHeader]

reqDto = userStateDto

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext =
  it "HTTP 200 OK" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      let expDto = reqDto
      let expType (a :: UserStateDTO) = a
      -- AND: Prepare DB
      runInContextIO (insertUserEmailLink registrationUserEmailLink) requestContext
      runInContextIO (updateUserByUuid (userAdmin {active = False})) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      assertResponse expStatus expHeaders expDto expType response
      -- AND: Find result in DB and compare with expectation state
      assertExistenceOfUserInDB requestContext userAdmin

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 requestContext = createInvalidJsonTest reqMethod reqUrl "active"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 requestContext =
  createNotFoundTest'
    reqMethod
    (reqUrlWithHash "c996414a-b51d-4c8c-bc10-5ee3dab85fa8")
    reqHeaders
    reqBody
    "user_email_link"
    [("hash", "c996414a-b51d-4c8c-bc10-5ee3dab85fa8"), ("type", "RegistrationUserEmailLinkType")]

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404_forgotten_password_hash requestContext =
  it "HTTP 404 NOT FOUND - a password reset hash does not confirm an account" $ do
    -- GIVEN: Prepare DB
    runInContextIO (insertUserEmailLink forgottenPasswordUserEmailLink) requestContext
    -- WHEN: Call API
    response <- request reqMethod (reqUrlWithHash forgottenPasswordUserEmailLink.hash) reqHeaders reqBody
    -- THEN: Compare response with expectation
    response `shouldRespondWith` notFoundMatcher forgottenPasswordUserEmailLink.hash

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404_used_hash requestContext =
  it "HTTP 404 NOT FOUND - a used hash cannot be replayed" $ do
    -- GIVEN: Prepare DB
    runInContextIO (insertUserEmailLink registrationUserEmailLink) requestContext
    runInContextIO (updateUserByUuid (userAdmin {active = False})) requestContext
    -- AND: Use the hash
    _ <- request reqMethod reqUrl reqHeaders reqBody
    -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- THEN: Compare response with expectation
    response `shouldRespondWith` notFoundMatcher registrationUserEmailLink.hash

notFoundMatcher hash =
  ResponseMatcher
    { matchHeaders = resCtHeader : resCorsHeaders
    , matchStatus = 404
    , matchBody =
        bodyEquals . encode $
          NotExistsError (_ERROR_DATABASE__ENTITY_NOT_FOUND "user_email_link" [("tenant_uuid", U.toString defaultTenantUuid), ("hash", hash), ("type", "RegistrationUserEmailLinkType")])
    }
