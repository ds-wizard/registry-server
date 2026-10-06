module Specs.Api.Handler.UserEmailLink.List_POST (
  list_POST,
) where

import Data.Aeson (encode)
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import RegistryServer.Api.Resource.UserEmailLink.UserEmailLinkJM ()
import RegistryServer.Database.Mapping.UserEmailLink.UserEmailLinkType ()
import RegistryServer.Database.Migration.Development.User.Data.Users
import RegistryServer.Database.Migration.Development.UserEmailLink.Data.UserEmailLinks
import RegistryServer.Model.Context.RequestContext
import RegistryServer.Model.User.User
import RegistryServer.Model.UserEmailLink.UserEmailLinkType
import Shared.Api.Resource.UserEmailLink.UserEmailLinkDTO
import Shared.Api.Resource.UserEmailLink.UserEmailLinkJM ()
import Shared.Database.DAO.UserEmailLink.UserEmailLinkDAO
import Shared.Model.UserEmailLink.UserEmailLink

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common

-- ------------------------------------------------------------------------
-- POST /api/user-email-links
-- ------------------------------------------------------------------------
list_POST :: RequestContext -> SpecWith ((), Application)
list_POST requestContext =
  describe "POST /api/user-email-links" $ do
    test_201 requestContext
    test_201_unknown_email requestContext
    test_400 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/user-email-links"

reqHeaders = [reqCtHeader]

reqDto = forgottenPasswordUserEmailLinkDto

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_201 requestContext =
  it "HTTP 201 CREATED" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 201
      let expHeaders = resCorsHeaders
      let expBody = ""
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find result in DB and compare with expectation state
      userEmailLinkFromDb <- getFirstFromDB findUserEmailLinks requestContext
      liftIO $ userEmailLinkFromDb.aType `shouldBe` reqDto.aType
      liftIO $ userEmailLinkFromDb.identity `shouldBe` U.toString userAdmin.uuid

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_201_unknown_email requestContext =
  it "HTTP 201 CREATED when email doesn't exist (nothing is sent)" $
    -- GIVEN: Prepare request
    do
      let reqDto = forgottenPasswordUserEmailLinkDto {email = "non-existing@example.com"} :: UserEmailLinkDTO UserEmailLinkType
      let reqBody = encode reqDto
      -- Prepare expectation
      let expStatus = 201
      let expHeaders = resCorsHeaders
      let expBody = ""
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find result in DB and compare with expectation state
      assertCountInDB (findUserEmailLinks :: RequestContextM [UserEmailLink String UserEmailLinkType]) requestContext 0

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 requestContext = createInvalidJsonTest reqMethod reqUrl "type"
