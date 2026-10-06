module Specs.Api.Handler.User.List_Current_Password_PUT (
  list_current_password_PUT,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai

import RegistryServer.Api.Resource.User.UserPasswordDTO
import RegistryServer.Api.Resource.User.UserPasswordJM ()
import RegistryServer.Database.DAO.User.UserDAO
import RegistryServer.Database.Migration.Development.User.Data.Users
import RegistryServer.Model.Context.RequestContext
import RegistryServer.Model.User.User
import Shared.Util.Password

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common

-- ------------------------------------------------------------------------
-- PUT /api/users/current/password
-- ------------------------------------------------------------------------
list_current_password_PUT :: RequestContext -> SpecWith ((), Application)
list_current_password_PUT requestContext =
  describe "PUT /api/users/current/password" $ do
    test_204 requestContext
    test_401 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqUrl = "/api/users/current/password"

reqHeaders = [reqUserAuthHeader, reqCtHeader]

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
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      assertEmptyResponse expStatus expHeaders response
      -- AND: Find result in DB and compare with expectation state
      userFromDb <- getOneFromDB (findUserByUuid userNikola.uuid) requestContext
      liftIO $ verifyPassword reqDto.password userFromDb.passwordHash `shouldBe` True

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody
