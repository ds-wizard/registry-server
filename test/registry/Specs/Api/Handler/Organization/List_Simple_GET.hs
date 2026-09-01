module Specs.Api.Handler.Organization.List_Simple_GET (
  list_simple_GET,
) where

import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai

import RegistryPublic.Api.Resource.Organization.OrganizationSimpleJM ()
import RegistryPublic.Database.Migration.Development.Organization.Data.Organizations
import RegistryPublic.Model.Organization.OrganizationSimple
import RegistryServer.Model.Context.RequestContext
import RegistryServer.Service.Organization.OrganizationMapper

import SharedTest.Specs.Api.Common

-- ------------------------------------------------------------------------
-- GET /organizations/simple
-- ------------------------------------------------------------------------
list_simple_GET :: RequestContext -> SpecWith ((), Application)
list_simple_GET requestContext = describe "GET /organizations/simple" $ test_200 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/organizations/simple"

reqHeaders = []

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
      let expDto = toSimpleDTO <$> [orgGlobal, orgNetherlands]
      let expType (a :: [OrganizationSimple]) = a
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      assertListResponse expStatus expHeaders expDto expType response
