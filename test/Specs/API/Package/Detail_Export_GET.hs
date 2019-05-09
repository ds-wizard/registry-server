module Specs.API.Package.Detail_Export_GET
  ( detail_export_get
  ) where

import Control.Lens ((^.))
import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Api.Resource.Error.ErrorDTO ()
import Api.Resource.Package.PackageDetailJM ()
import Database.Migration.Development.KnowledgeModelBundle.Data.KnowledgeModelBundles
import Database.Migration.Development.Package.Data.Packages
import LensesConfig
import Model.Context.AppContext
import Service.KnowledgeModelBundle.KnowledgeModelBundleMapper

import Specs.API.Common

-- ------------------------------------------------------------------------
-- GET /packages/{pkgId}/export
-- ------------------------------------------------------------------------
detail_export_get :: AppContext -> SpecWith Application
detail_export_get appContext =
  describe "GET /packages/{pkgId}/export" $ do
    test_200 appContext
    test_401 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = BS.pack $ "/packages/" ++ (netherlandsPackageV2 ^. pId) ++ "/export"

reqHeaders = [reqAdminAuthHeader, reqCtHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext = do
  it "HTTP 200 OK" $
     -- GIVEN: Prepare expectation
   do
    let expStatus = 200
    let expHeaders = [resCtHeader] ++ resCorsHeaders
    let expDto = toDTO netherlandsPackageV2KMBudle
    let expBody = encode expDto
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
     -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod reqUrl [] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 appContext =
  createNotFoundTest
    reqMethod
    "/packages/dsw.global:non-existing-package:1.0.0/export"
    reqHeaders
    reqBody
    "package"
    "dsw.global:non-existing-package:1.0.0"
