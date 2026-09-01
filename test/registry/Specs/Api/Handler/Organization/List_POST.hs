module Specs.Api.Handler.Organization.List_POST (
  list_POST,
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
import RegistryPublic.Model.Organization.OrganizationRole
import RegistryServer.Database.DAO.Organization.OrganizationDAO
import RegistryServer.Localization.Messages.Public
import RegistryServer.Model.Context.RequestContext
import RegistryServer.Service.Organization.OrganizationMapper
import Shared.Localization.Messages.Coordinate.Public
import Shared.Model.Error.Error

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.Organization.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- POST /organizations
-- ------------------------------------------------------------------------
list_POST :: RequestContext -> SpecWith ((), Application)
list_POST requestContext =
  describe "POST /organizations" $ do
    test_201 requestContext
    test_400_invalid_json requestContext
    test_400_invalid_organizationId requestContext
    test_400_organizationId_duplication requestContext
    test_400_email_duplication requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/organizations"

reqHeaders = [reqCtHeader]

reqDto = orgGlobalCreate

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_201 requestContext =
  it "HTTP 201 CREATED" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 201
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      let expDto = toDTO (orgGlobal {active = False, logo = Nothing, oRole = UserRole})
      let expType (a :: OrganizationDTO) = a
      -- AND: Prepare DB
      runInContextIO deleteOrganizations requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      assertResponseWithoutFields expStatus expHeaders expDto expType response ["token", "createdAt", "updatedAt"]
      -- AND: Find result in DB and compare with expectation state
      organizationFromDb <- getFirstFromDB findOrganizations requestContext
      compareOrganizationDtosWhenCreate organizationFromDb reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400_invalid_json requestContext = createInvalidJsonTest reqMethod reqUrl "organizationId"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400_invalid_organizationId requestContext =
  it "HTTP 400 BAD REQUEST when organizationId is not in valid format" $
    -- GIVEN: Prepare request
    do
      let reqDto = orgGlobalCreate {organizationId = "organization:amsterdam"} :: OrganizationCreateDTO
      let reqBody = encode reqDto
      -- AND: Prepare expectation
      let expStatus = 400
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      let expDto = ValidationError [] (M.singleton "organizationId" [_ERROR_VALIDATION__INVALID_COORDINATE_PART_FORMAT "organizationId" "organization:amsterdam"])
      let expType (a :: AppError) = a
      -- AND: Prepare DB
      runInContextIO deleteOrganizations requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      assertResponse expStatus expHeaders expDto expType response
      -- AND: Find result in DB and compare with expectation state
      assertCountInDB findOrganizations requestContext 0

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400_organizationId_duplication requestContext =
  it "HTTP 400 BAD REQUEST when organizationId is already used" $
    -- GIVEN: Prepare request
    do
      let orgId = orgGlobalCreate.organizationId
      -- AND: Prepare expectation
      let expStatus = 400
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      let expDto = ValidationError [] (M.singleton "organizationId" [_ERROR_VALIDATION__ORGANIZATION_ID_UNIQUENESS orgId])
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
test_400_email_duplication requestContext =
  it "HTTP 400 BAD REQUEST when email is already used" $
    -- GIVEN: Prepare request
    do
      let orgEmail = orgGlobalCreate.email
      let reqDto = orgGlobalCreate {organizationId = "org.de"} :: OrganizationCreateDTO
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
