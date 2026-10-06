module Specs.Api.Handler.KnowledgeModelPackage.List_Bundle_POST (
  list_bundle_POST,
) where

import Data.Aeson (Value (..), encode, toJSON)
import qualified Data.Map.Strict as M
import Network.HTTP.Types
import Network.Wai (Application)
import Network.Wai.Test hiding (request)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import RegistryServer.Database.DAO.Publication.PublicationDAO
import RegistryServer.Database.Migration.Development.User.Data.Users
import RegistryServer.Model.Context.RequestContext
import RegistryServer.Model.User.User
import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Api.Resource.KnowledgeModel.Bundle.KnowledgeModelBundleJM ()
import Shared.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.Database.Migration.Development.KnowledgeModel.Data.Bundle.KnowledgeModelBundles
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.Localization.Messages.Coordinate.Public
import Shared.Model.Coordinate.Coordinate
import Shared.Model.Error.Error
import Shared.Model.KnowledgeModel.Bundle.KnowledgeModelBundle
import Shared.Model.KnowledgeModel.Bundle.KnowledgeModelBundlePackage
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- POST /api/knowledge-model-packages/bundle
-- ------------------------------------------------------------------------
list_bundle_POST :: RequestContext -> SpecWith ((), Application)
list_bundle_POST requestContext =
  describe "POST /api/knowledge-model-packages/bundle" $ do
    test_200 requestContext
    test_200_legacy requestContext
    test_400 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/knowledge-model-packages/bundle"

reqHeaders = [reqAdminAuthHeader, reqCtHeader]

reqBody = encode netherlandsV2KmBundle

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext =
  it "HTTP 200 OK" $ do
    -- GIVEN: Prepare DB
    runInContextIO deletePackages requestContext
    -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- THEN: Compare response with expectation
    let (SResponse (Status status _) _ _) = response
    liftIO $ status `shouldBe` 200
    -- AND: Find result in DB and compare with expectation state
    pkg <- getOneFromDB (findPackageByCoordinate (createCoordinate netherlandsKmPackageV2) Nothing) requestContext
    createdBy <- getOneFromDB (findPublicationCreatedBy pkg.uuid) requestContext
    liftIO $ createdBy `shouldBe` Just userAdmin.uuid

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200_legacy requestContext =
  it "HTTP 200 OK (bundle with organizationId and kmId)" $ do
    -- GIVEN: Prepare DB
    runInContextIO deletePackages requestContext
    -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders (encode . toLegacyBundle . toJSON $ netherlandsV2KmBundle)
    -- THEN: Compare response with expectation
    let (SResponse (Status status _) _ _) = response
    liftIO $ status `shouldBe` 200
    -- AND: Find result in DB and compare with expectation state
    pkg <- getOneFromDB (findPackageByCoordinate (createCoordinate netherlandsKmPackageV2) Nothing) requestContext
    liftIO $ pkg.id `shouldBe` netherlandsKmPackageV2.id
    liftIO $ pkg.forkOfPackageId `shouldBe` netherlandsKmPackageV2.forkOfPackageId

toLegacyBundle :: Value -> Value
toLegacyBundle (Object o) = Object . adjustKey "packages" toLegacyPackages . toLegacyIdFields "organizationId" "kmId" $ o
toLegacyBundle value = value

toLegacyPackages :: Value -> Value
toLegacyPackages (Array packages) = Array (fmap toLegacyPackage packages)
toLegacyPackages value = value

toLegacyPackage :: Value -> Value
toLegacyPackage (Object o) =
  Object
    . toLegacyIdFields "organizationId" "kmId"
    . toLegacyReferenceField "previousPackage"
    . toLegacyReferenceField "forkOfPackage"
    . toLegacyReferenceField "mergeCheckpointPackage"
    $ o
toLegacyPackage value = value

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 requestContext =
  it "HTTP 400 BAD REQUEST when id is not in valid format" $
    -- GIVEN: Prepare request
    do
      let mainPackage = netherlandsV2KmBundlePackage {id = "a:b"} :: KnowledgeModelBundlePackage
      let reqDto = netherlandsV2KmBundle {id = "a:b", packages = [globalKmBundlePackage, netherlandsKmBundlePackage, mainPackage]} :: KnowledgeModelBundle
      let reqBody = encode reqDto
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
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createForbiddenTest reqMethod reqUrl [reqUserAuthHeader, reqCtHeader] reqBody "Write KnowledgeModelBundle"
