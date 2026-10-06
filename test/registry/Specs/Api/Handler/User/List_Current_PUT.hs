module Specs.Api.Handler.User.List_Current_PUT (
  list_current_PUT,
) where

import Data.Aeson (encode)
import qualified Data.Map.Strict as M
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import RegistryServer.Api.Resource.User.UserDTO
import RegistryServer.Api.Resource.User.UserJM ()
import RegistryServer.Api.Resource.User.UserProfileChangeDTO
import RegistryServer.Api.Resource.User.UserProfileChangeJM ()
import RegistryServer.Database.Migration.Development.User.Data.Users
import RegistryServer.Localization.Messages.Public
import RegistryServer.Model.Context.RequestContext
import RegistryServer.Model.User.User
import RegistryServer.Service.User.UserMapper
import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Model.Error.Error

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.User.Common

-- ------------------------------------------------------------------------
-- PUT /api/users/current
-- ------------------------------------------------------------------------
list_current_PUT :: RequestContext -> SpecWith ((), Application)
list_current_PUT requestContext =
  describe "PUT /api/users/current" $ do
    test_200 requestContext
    test_400 requestContext
    test_401 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqUrl = "/api/users/current"

reqHeaders = [reqAdminAuthHeader, reqCtHeader]

reqDto = userAdminProfileChange

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
      let expUser = userAdmin {email = reqDto.email, firstName = reqDto.firstName, lastName = reqDto.lastName} :: User
      let expDto = toDTO expUser
      let expType (a :: UserDTO) = a
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      assertResponseWithoutFields expStatus expHeaders expDto expType response ["updatedAt"]
      -- AND: Find result in DB and compare with expectation state
      assertExistenceOfUserInDB requestContext expUser

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 requestContext = do
  createInvalidJsonTest reqMethod reqUrl "email"
  it "HTTP 400 BAD REQUEST when email is already used" $
    -- GIVEN: Prepare request
    do
      let reqDto = userAdminProfileChange {email = userNikola.email} :: UserProfileChangeDTO
      let reqBody = encode reqDto
      -- AND: Prepare expectation
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = ValidationError [] (M.singleton "email" [_ERROR_VALIDATION__USER_EMAIL_UNIQUENESS userNikola.email])
      let expBody = encode expDto
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody
