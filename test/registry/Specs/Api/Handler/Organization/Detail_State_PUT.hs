module Specs.Api.Handler.Organization.Detail_State_PUT (
  detail_state_PUT,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai

import RegistryPublic.Api.Resource.Organization.OrganizationDTO
import RegistryPublic.Api.Resource.Organization.OrganizationJM ()
import RegistryPublic.Api.Resource.Organization.OrganizationStateJM ()
import RegistryPublic.Database.Migration.Development.Organization.Data.Organizations
import RegistryPublic.Model.Organization.Organization
import RegistryServer.Database.DAO.Organization.OrganizationDAO
import RegistryServer.Database.Migration.Development.UserEmailLink.Data.UserEmailLinks
import RegistryServer.Model.Context.RequestContext
import RegistryServer.Service.Organization.OrganizationMapper
import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Database.DAO.UserEmailLink.UserEmailLinkDAO

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.Organization.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- PUT /organizations/{orgId}/state
-- ------------------------------------------------------------------------
detail_state_PUT :: RequestContext -> SpecWith ((), Application)
detail_state_PUT requestContext =
  describe "PUT /organizations/{orgId}/state" $ do
    test_200 requestContext
    test_400 requestContext
    test_404 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqUrl = "/organizations/global/state?hash=1ba90a0f-845e-41c7-9f1c-a55fc5a0554a"

reqHeaders = [reqCtHeader]

reqDto = orgStateDto

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
      let expDto = toDTO orgGlobal
      let expType (a :: OrganizationDTO) = a
      -- AND: Prepare DB
      runInContextIO (insertUserEmailLink registrationUserEmailLink) requestContext
      runInContextIO (updateOrganization (orgGlobal {active = False})) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      assertResponseWithoutFields expStatus expHeaders expDto expType response ["updatedAt"]
      -- AND: Find result in DB and compare with expectation state
      assertExistenceOfOrganizationInDB requestContext orgGlobal

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
    "/organizations/global/state?hash=c996414a-b51d-4c8c-bc10-5ee3dab85fa8"
    reqHeaders
    reqBody
    "user_email_link"
    [("hash", "c996414a-b51d-4c8c-bc10-5ee3dab85fa8")]
