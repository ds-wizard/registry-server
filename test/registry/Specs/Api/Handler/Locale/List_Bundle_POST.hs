module Specs.Api.Handler.Locale.List_Bundle_POST (
  list_bundle_POST,
) where

import Codec.Archive.Zip
import Data.Aeson (Value (..), encode, toJSON)
import qualified Data.Map.Strict as M
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import RegistryPublic.Api.Resource.Locale.LocaleDTO
import RegistryPublic.Api.Resource.Locale.LocaleJM ()
import RegistryServer.Database.DAO.Publication.PublicationDAO
import RegistryServer.Database.Migration.Development.User.Data.Users
import RegistryServer.Model.Context.RequestContext
import RegistryServer.Model.User.User
import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Api.Resource.LocaleBundle.LocaleBundleJM ()
import Shared.Database.DAO.Locale.LocaleDAO
import Shared.Database.Migration.Development.Locale.Data.Locales
import Shared.Localization.Messages.Coordinate.Public
import Shared.Model.Error.Error
import Shared.Model.Locale.Locale
import Shared.Service.Locale.Bundle.LocaleBundleMapper

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common

-- ------------------------------------------------------------------------
-- POST /api/locales/bundle
-- ------------------------------------------------------------------------
list_bundle_POST :: RequestContext -> SpecWith ((), Application)
list_bundle_POST requestContext =
  describe "POST /api/locales/bundle" $ do
    test_201 requestContext
    test_201_legacy requestContext
    test_400 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/locales/bundle"

reqHeaders = [reqAdminAuthHeader, reqCtMultipartHeader]

reqBody = createMultipartBody "locale.zip" "application/zip" (toLocaleArchive localeNl localeNlContent localeNlContent)

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
      let (status, headers, resDto) = destructResponse response :: (Int, ResponseHeaders, LocaleDTO)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      liftIO $ resDto.id `shouldBe` localeNl.id
      -- AND: Find result in DB and compare with expectation state
      localeFromDb <- getOneFromDB (findLocaleByUuid resDto.uuid) requestContext
      liftIO $ localeFromDb.id `shouldBe` localeNl.id
      createdBy <- getOneFromDB (findPublicationCreatedBy resDto.uuid) requestContext
      liftIO $ createdBy `shouldBe` Just userAdmin.uuid

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_201_legacy requestContext =
  it "HTTP 201 CREATED (bundle with organizationId and localeId)" $
    -- GIVEN: Prepare request
    do
      let legacyBundle = toLegacyBundle . toJSON . toLocaleBundle $ localeNl
      let archive =
            fromArchive $
              foldr
                addEntryToArchive
                emptyArchive
                [toEntry "locale/locale.json" 0 (encode legacyBundle), toTranslationEntry "wizard.json" localeNlContent, toTranslationEntry "mail.po" localeNlContent]
      let reqBody = createMultipartBody "locale.zip" "application/zip" archive
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, _, resDto) = destructResponse response :: (Int, ResponseHeaders, LocaleDTO)
      assertResStatus status 201
      -- AND: Find result in DB and compare with expectation state
      localeFromDb <- getOneFromDB (findLocaleByUuid resDto.uuid) requestContext
      liftIO $ localeFromDb.id `shouldBe` localeNl.id
      liftIO $ localeFromDb.version `shouldBe` localeNl.version

toLegacyBundle :: Value -> Value
toLegacyBundle (Object o) = Object (toLegacyIdFields "organizationId" "localeId" o)
toLegacyBundle value = value

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 requestContext =
  it "HTTP 400 BAD REQUEST when id is not in valid format" $
    -- GIVEN: Prepare request
    do
      let reqBody = createMultipartBody "locale.zip" "application/zip" (toLocaleArchive (localeNl {id = "a:b"} :: Locale) localeNlContent localeNlContent)
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
test_403 requestContext = createForbiddenTest reqMethod reqUrl [reqUserAuthHeader, reqCtMultipartHeader] reqBody "Write LocaleBundle"
