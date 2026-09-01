module Specs.Api.Handler.Organization.Detail_Token_PUT (
  detail_token_PUT,
) where

import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai

import RegistryPublic.Api.Resource.Organization.OrganizationDTO
import RegistryPublic.Api.Resource.Organization.OrganizationJM ()
import RegistryPublic.Database.Migration.Development.Organization.Data.Organizations
import RegistryPublic.Model.Organization.Organization
import RegistryServer.Database.DAO.Organization.OrganizationDAO
import RegistryServer.Database.Migration.Development.UserEmailLink.Data.UserEmailLinks
import RegistryServer.Model.Context.RequestContext
import RegistryServer.Service.Organization.OrganizationMapper
import Shared.Database.DAO.UserEmailLink.UserEmailLinkDAO

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- PUT /organizations/{orgId}/token
-- ------------------------------------------------------------------------
detail_token_PUT :: RequestContext -> SpecWith ((), Application)
detail_token_PUT requestContext =
  describe "PUT /organizations/{orgId}/token" $ do
    test_200 requestContext
    test_404 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqUrl = "/organizations/global/token?hash=5b1aff0d-b5e3-436d-b913-6b52d3cbad5f"

reqHeaders = [reqCtHeader]

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
      let expDto = toDTO orgGlobal
      let expType (a :: OrganizationDTO) = a
      -- AND: Prepare DB
      runInContextIO (insertUserEmailLink forgottenTokenUserEmailLink) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      assertResponseWithoutFields expStatus expHeaders expDto expType response ["token", "updatedAt"]
      -- AND: Find result in DB and compare with expectation state
      orgFromDb <- getFirstFromDB findOrganizations requestContext
      liftIO $ (orgFromDb.token /= orgGlobal.token) `shouldBe` True

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 requestContext =
  createNotFoundTest'
    reqMethod
    "/organizations/global/token?hash=c996414a-b51d-4c8c-bc10-5ee3dab85fa8"
    reqHeaders
    reqBody
    "user_email_link"
    [("hash", "c996414a-b51d-4c8c-bc10-5ee3dab85fa8")]
