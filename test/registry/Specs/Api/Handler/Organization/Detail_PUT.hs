module Specs.Api.Handler.Organization.Detail_PUT (
  detail_PUT,
) where

import Data.Aeson (encode)
import qualified Data.Map.Strict as M
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai

import RegistryPublic.Api.Resource.Organization.OrganizationCreateDTO
import RegistryPublic.Api.Resource.Organization.OrganizationCreateJM ()
import RegistryPublic.Api.Resource.Organization.OrganizationDTO
import RegistryPublic.Api.Resource.Organization.OrganizationJM ()
import RegistryPublic.Database.Migration.Development.Organization.Data.Organizations
import RegistryPublic.Model.Organization.Organization
import RegistryServer.Api.Resource.Organization.OrganizationChangeJM ()
import RegistryServer.Database.DAO.Organization.OrganizationDAO
import RegistryServer.Database.Migration.Development.Organization.Data.Organizations
import RegistryServer.Localization.Messages.Public
import RegistryServer.Model.Context.RequestContext
import RegistryServer.Service.Organization.OrganizationMapper
import Shared.Model.Error.Error

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.Organization.Common

-- ------------------------------------------------------------------------
-- PUT /organizations/{orgId}
-- ------------------------------------------------------------------------
detail_PUT :: RequestContext -> SpecWith ((), Application)
detail_PUT requestContext =
  describe "PUT /organizations/{orgId}" $ do
    test_200 requestContext
    test_400 requestContext
    test_401 requestContext
    test_403 requestContext
    test_404 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqUrl = "/organizations/global"

reqHeaders = [reqCtHeader, reqAdminAuthHeader]

reqDto = orgGlobalEditedChange

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
      let expDto = toDTO orgGlobalEdited
      let expType (a :: OrganizationDTO) = a
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      assertResponseWithoutFields expStatus expHeaders expDto expType response ["updatedAt"]
      -- AND: Find result in DB and compare with expectation state
      assertExistenceOfOrganizationInDB requestContext orgGlobalEdited

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 requestContext = do
  createInvalidJsonTest reqMethod reqUrl "description"
  it "HTTP 400 BAD REQUEST when email is already used" $
    -- GIVEN: Prepare request
    do
      let orgEmail = orgNetherlands.email
      let reqDto = orgGlobalCreate {email = orgEmail} :: OrganizationCreateDTO
      let reqBody = encode reqDto
      -- AND: Prepare expectation
      let expStatus = 400
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      let expDto = ValidationError [] (M.singleton "email" [_ERROR_VALIDATION__ORGANIZATION_EMAIL_UNIQUENESS orgEmail])
      let expType (a :: AppError) = a
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      assertResponse expStatus expHeaders expDto expType response
      -- AND: Find result in DB and compare with expectation state
      assertCountInDB findOrganizations requestContext 2

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext =
  createForbiddenTest reqMethod reqUrl [reqCtHeader, reqUserAuthHeader] reqBody "Detail Organization"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 requestContext =
  createNotFoundTest
    reqMethod
    "/organizations/nonexisting.organization"
    reqHeaders
    reqBody
    "organization"
    [("organization_id", "nonexisting.organization")]
