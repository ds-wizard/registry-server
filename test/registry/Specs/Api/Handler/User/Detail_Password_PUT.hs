module Specs.Api.Handler.User.Detail_Password_PUT (
  detail_password_PUT,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai

import RegistryServer.Api.Resource.User.UserPasswordDTO
import RegistryServer.Api.Resource.User.UserPasswordJM ()
import RegistryServer.Database.DAO.User.UserDAO
import RegistryServer.Database.Mapping.UserEmailLink.UserEmailLinkType ()
import RegistryServer.Database.Migration.Development.User.Data.Users
import RegistryServer.Database.Migration.Development.UserEmailLink.Data.UserEmailLinks
import RegistryServer.Model.Context.RequestContext
import RegistryServer.Model.User.User
import Shared.Database.DAO.UserEmailLink.UserEmailLinkDAO
import Shared.Util.Password

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- PUT /api/users/{uuid}/password
-- ------------------------------------------------------------------------
detail_password_PUT :: RequestContext -> SpecWith ((), Application)
detail_password_PUT requestContext =
  describe "PUT /api/users/{uuid}/password" $ do
    test_204 requestContext
    test_204_by_admin requestContext
    test_404 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqUrl = "/api/users/4e2a6c9f-3d7b-4b8e-9c1a-5f0d2e8b7a61/password?hash=5b1aff0d-b5e3-436d-b913-6b52d3cbad5f"

reqHeaders = [reqCtHeader]

reqDto = userPasswordDto

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_204 requestContext =
  it "HTTP 204 NO CONTENT" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 204
      let expHeaders = resCorsHeadersPlain
      -- AND: Prepare DB
      runInContextIO (insertUserEmailLink forgottenPasswordUserEmailLink) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      assertEmptyResponse expStatus expHeaders response
      -- AND: Find result in DB and compare with expectation state
      userFromDb <- getOneFromDB (findUserByUuid userAdmin.uuid) requestContext
      liftIO $ verifyPassword reqDto.password userFromDb.passwordHash `shouldBe` True

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_204_by_admin requestContext =
  it "HTTP 204 NO CONTENT (by admin, without a hash)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 204
      let expHeaders = resCorsHeadersPlain
      -- WHEN: Call API
      response <- request reqMethod "/api/users/7b1f0d3e-8a2c-4f6b-b5d9-2c4e6a8f0b13/password" [reqAdminAuthHeader, reqCtHeader] reqBody
      -- THEN: Compare response with expectation
      assertEmptyResponse expStatus expHeaders response
      -- AND: Find result in DB and compare with expectation state
      userFromDb <- getOneFromDB (findUserByUuid userNikola.uuid) requestContext
      liftIO $ verifyPassword reqDto.password userFromDb.passwordHash `shouldBe` True

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 requestContext =
  createNotFoundTest'
    reqMethod
    "/api/users/4e2a6c9f-3d7b-4b8e-9c1a-5f0d2e8b7a61/password?hash=c996414a-b51d-4c8c-bc10-5ee3dab85fa8"
    reqHeaders
    reqBody
    "user_email_link"
    [("hash", "c996414a-b51d-4c8c-bc10-5ee3dab85fa8"), ("type", "ForgottenPasswordUserEmailLinkType")]
