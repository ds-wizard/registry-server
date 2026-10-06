module Specs.Api.Handler.DocumentTemplate.List_GET (
  list_GET,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import RegistryPublic.Api.Resource.DocumentTemplate.DocumentTemplateSimpleJM ()
import RegistryServer.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import qualified RegistryServer.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML_Migration
import RegistryServer.Model.Context.RequestContext

import RegistryPublic.Api.Resource.DocumentTemplate.DocumentTemplateSimpleDTO
import SharedTest.Specs.Api.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- GET /api/document-templates
-- ------------------------------------------------------------------------
list_GET :: RequestContext -> SpecWith ((), Application)
list_GET requestContext = describe "GET /api/document-templates" $ test_200 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/api/document-templates"

reqHeaders = [reqCtHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  create_test_200 "HTTP 200 OK" requestContext "/api/document-templates" [wizardDocumentTemplateSimpleDTO]
  create_test_200 "HTTP 200 OK (metamodelVersion=99.0)" requestContext "/api/document-templates?metamodelVersion=99.0" [wizardDocumentTemplateSimpleDTO]
  create_test_200 "HTTP 200 OK (metamodelVersion=10.0)" requestContext "/api/document-templates?metamodelVersion=10.0" ([] :: [DocumentTemplateSimpleDTO])

create_test_200 title requestContext reqUrl expDto =
  it title $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO TML_Migration.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
