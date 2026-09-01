module Specs.Api.Handler.UserEmailLink.List_POST (
  list_POST,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import RegistryPublic.Database.Migration.Development.Organization.Data.Organizations
import RegistryPublic.Model.Organization.Organization
import RegistryServer.Api.Resource.UserEmailLink.UserEmailLinkJM ()
import RegistryServer.Database.Migration.Development.UserEmailLink.Data.UserEmailLinks
import RegistryServer.Localization.Messages.Public
import RegistryServer.Model.Context.RequestContext
import RegistryServer.Model.UserEmailLink.UserEmailLinkType
import Shared.Api.Resource.UserEmailLink.UserEmailLinkDTO
import Shared.Api.Resource.UserEmailLink.UserEmailLinkJM ()
import Shared.Database.DAO.UserEmailLink.UserEmailLinkDAO
import Shared.Model.Error.Error
import Shared.Model.UserEmailLink.UserEmailLink

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common

-- ------------------------------------------------------------------------
-- POST /user-email-links
-- ------------------------------------------------------------------------
list_POST :: RequestContext -> SpecWith ((), Application)
list_POST requestContext =
  describe "POST /user-email-links" $ do
    test_201 requestContext
    test_400 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/user-email-links"

reqHeaders = [reqCtHeader]

reqDto = forgottenTokenUserEmailLinkDto

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
      liftIO $ userEmailLinkFromDb.identity `shouldBe` orgGlobal.organizationId

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 requestContext = do
  createInvalidJsonTest reqMethod reqUrl "type"
  it "HTTP 400 BAD REQUEST when email doesn't exist" $
    -- GIVEN: Prepare request
    do
      let reqDto = forgottenTokenUserEmailLinkDto {email = "non-existing@example.com"} :: UserEmailLinkDTO UserEmailLinkType
      let reqBody = encode reqDto
      -- Prepare expectation
      let expStatus = 400
      let expHeaders = resCorsHeaders
      let expDto = UserError $ _ERROR_VALIDATION__ORGANIZATION_EMAIL_ABSENCE "non-existing@example.com"
      let expBody = encode expDto
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
