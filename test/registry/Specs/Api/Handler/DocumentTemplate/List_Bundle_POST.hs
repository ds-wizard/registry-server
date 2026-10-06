module Specs.Api.Handler.DocumentTemplate.List_Bundle_POST (
  list_bundle_POST,
) where

import Codec.Archive.Zip
import Data.Aeson (Value (..), decode, encode)
import qualified Data.Map.Strict as M
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import RegistryServer.Database.DAO.Publication.PublicationDAO
import RegistryServer.Database.Migration.Development.User.Data.Users
import RegistryServer.Model.Context.RequestContext
import RegistryServer.Model.User.User
import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import Shared.Localization.Messages.Coordinate.Public
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.DocumentTemplate.DocumentTemplateJM ()
import Shared.Model.DocumentTemplate.DocumentTemplateSimple
import Shared.Model.Error.Error
import Shared.Service.DocumentTemplate.Bundle.DocumentTemplateBundleMapper

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common

-- ------------------------------------------------------------------------
-- POST /api/document-templates/bundle
-- ------------------------------------------------------------------------
list_bundle_POST :: RequestContext -> SpecWith ((), Application)
list_bundle_POST requestContext =
  describe "POST /api/document-templates/bundle" $ do
    test_201 requestContext
    test_201_legacy requestContext
    test_400 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/document-templates/bundle"

reqHeaders = [reqAdminAuthHeader, reqCtMultipartHeader]

reqBody = createMultipartBody "template.zip" "application/zip" (toDocumentTemplateArchive (toBundle wizardDocumentTemplate [] [] []) [])

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_201 requestContext =
  it "HTTP 201 CREATED" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 201
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resDto) = destructResponse response :: (Int, ResponseHeaders, DocumentTemplateSimple)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      liftIO $ resDto.name `shouldBe` wizardDocumentTemplate.name
      -- AND: Find result in DB and compare with expectation state
      templateFromDb <- getOneFromDB (findDocumentTemplateByUuid resDto.uuid) requestContext
      liftIO $ templateFromDb.id `shouldBe` wizardDocumentTemplate.id
      createdBy <- getOneFromDB (findPublicationCreatedBy resDto.uuid) requestContext
      liftIO $ createdBy `shouldBe` Just userAdmin.uuid

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_201_legacy requestContext =
  it "HTTP 201 CREATED (bundle with organizationId and templateId)" $
    -- GIVEN: Prepare request
    do
      let Just legacyBundle = toLegacyBundle <$> (decode . fromEntry =<< findEntryByPath "template/template.json" (toArchive (toDocumentTemplateArchive (toBundle wizardDocumentTemplate [] [] []) [])))
      let archive = fromArchive $ addEntryToArchive (toEntry "template/template.json" 0 (encode legacyBundle)) emptyArchive
      let reqBody = createMultipartBody "template.zip" "application/zip" archive
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, _, resDto) = destructResponse response :: (Int, ResponseHeaders, DocumentTemplateSimple)
      assertResStatus status 201
      -- AND: Find result in DB and compare with expectation state
      templateFromDb <- getOneFromDB (findDocumentTemplateByUuid resDto.uuid) requestContext
      liftIO $ templateFromDb.id `shouldBe` wizardDocumentTemplate.id
      liftIO $ templateFromDb.version `shouldBe` wizardDocumentTemplate.version

toLegacyBundle :: Value -> Value
toLegacyBundle (Object o) = Object . adjustKey "allowedPackages" toLegacyPatterns . toLegacyIdFields "organizationId" "templateId" $ o
toLegacyBundle value = value

toLegacyPatterns :: Value -> Value
toLegacyPatterns (Array patterns) = Array (fmap toLegacyPattern patterns)
toLegacyPatterns value = value

toLegacyPattern :: Value -> Value
toLegacyPattern (Object o) = Object (toLegacyIdFields "orgId" "kmId" o)
toLegacyPattern value = value

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 requestContext =
  it "HTTP 400 BAD REQUEST when id is not in valid format" $
    -- GIVEN: Prepare request
    do
      let reqBody = createMultipartBody "template.zip" "application/zip" (toDocumentTemplateArchive (toBundle (wizardDocumentTemplate {id = "a:b"} :: DocumentTemplate) [] [] []) [])
      -- AND: Prepare expectation
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expBody = encode (ValidationError [] (M.singleton "id" [_ERROR_VALIDATION__INVALID_COORDINATE_PART_FORMAT "id" "a:b"]))
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtMultipartHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createForbiddenTest reqMethod reqUrl [reqUserAuthHeader, reqCtMultipartHeader] reqBody "Write DocumentTemplateBundle"
