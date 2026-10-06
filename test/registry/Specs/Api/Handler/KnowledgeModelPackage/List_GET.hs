module Specs.Api.Handler.KnowledgeModelPackage.List_GET (
  list_GET,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import RegistryServer.Database.DAO.Audit.AuditEntryDAO
import RegistryServer.Database.Migration.Development.Audit.Data.AuditEntries
import RegistryServer.Model.Context.RequestContext
import RegistryServer.Service.KnowledgeModel.Package.KnowledgeModelPackageMapper
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Audit.Common
import Specs.Api.Handler.Common

-- ------------------------------------------------------------------------
-- GET /api/knowledge-model-packages
-- ------------------------------------------------------------------------
list_GET :: RequestContext -> SpecWith ((), Application)
list_GET requestContext = describe "GET /api/knowledge-model-packages" $ test_200 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/api/knowledge-model-packages"

reqHeaders = [reqCtHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  it "HTTP 200 OK (Without Audit)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto =
            [toSimpleDTO globalKmPackage, toSimpleDTO netherlandsKmPackageV2]
      let expBody = encode expDto
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find result in DB and compare with expectation state
      assertCountInDB findAuditEntries requestContext 0
  it "HTTP 200 OK (With Audit)" $
    -- GIVEN: Prepare request
    do
      let reqHeaders = [reqCtHeader, reqAdminAuthHeader] ++ reqStatisticsHeader
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto =
            [toSimpleDTO globalKmPackage, toSimpleDTO netherlandsKmPackageV2]
      let expBody = encode expDto
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find result in DB and compare with expectation state
      assertExistenceOfAuditEntryInDB requestContext listPackagesAuditEntry
