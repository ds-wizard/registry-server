module Specs.Api.Handler.Common where

import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.CaseInsensitive as CI
import Data.Either (isRight)
import qualified Data.List as L
import Network.HTTP.Types
import Network.Wai (Application)
import Servant (serve)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import RegistryServer.Api.Middleware.LoggingMiddleware
import RegistryServer.Api.Web
import RegistryServer.Database.Migration.Development.Statistics.Data.InstanceStatistics
import RegistryServer.Model.Config.ServerConfig
import RegistryServer.Model.Context.RequestContext
import RegistryServer.Model.Context.ServerContext
import RegistryServer.Model.Statistics.InstanceStatistics
import Shared.Bootstrap.Web
import Shared.Constant.Api
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error

import SharedTest.Specs.Api.Common
import Specs.Common

startWebApp :: ServerContext -> RequestContext -> IO Application
startWebApp serverContext requestContext = do
  let config = requestContext.serverConfig
  let webPort = config.general.serverPort
  let env = config.general.environment
  return $ runMiddleware env loggingMiddleware $ serve webApi (webServer serverContext)

reqAdminAuthHeader :: Header
reqAdminAuthHeader = ("Authorization", "Bearer GlobalToken")

reqUserAuthHeader :: Header
reqUserAuthHeader = ("Authorization", "Bearer NetherlandsToken")

reqStatisticsHeader :: [Header]
reqStatisticsHeader =
  [ (CI.mk . BS.pack $ xUserCountHeaderName, BS.pack . show $ iStat.userCount)
  , (CI.mk . BS.pack $ xKnowledgeModelPackageCountHeaderName, BS.pack . show $ iStat.pkgCount)
  , (CI.mk . BS.pack $ xProjectCountHeaderName, BS.pack . show $ iStat.prjCount)
  , (CI.mk . BS.pack $ xKnowledgeModelEditorCountHeaderName, BS.pack . show $ iStat.kmEditorCount)
  , (CI.mk . BS.pack $ xDocCountHeaderName, BS.pack . show $ iStat.docCount)
  , (CI.mk . BS.pack $ xTmlCountHeaderName, BS.pack . show $ iStat.tmlCount)
  ]

-- ----------------------------------------------------
-- TESTS
-- ----------------------------------------------------
createInvalidJsonTest reqMethod reqUrl missingField =
  it "HTTP 400 BAD REQUEST when json is not valid" $ do
    let reqHeaders = [reqAdminAuthHeader, reqCtHeader]
    let reqBody = BSL.pack "{}"
    -- GIVEN: Prepare expectation
    let expStatus = 400
    let expHeaders = resCtHeaderUtf8 : resCorsHeaders
    -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyContainsInvalidJsonMessage}
    response `shouldRespondWith` responseMatcher

createForbiddenTest reqMethod reqUrl reqHeaders reqBody forbiddenReason =
  it "HTTP 403 FORBIDDEN" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 403
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = ForbiddenError (_ERROR_VALIDATION__FORBIDDEN forbiddenReason)
      let expBody = encode expDto
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ASSERT
-- ----------------------------------------------------
assertCountInDB dbFunction requestContext count = do
  eitherList <- runInContextIO dbFunction requestContext
  liftIO $ isRight eitherList `shouldBe` True
  let (Right list) = eitherList
  liftIO $ L.length list `shouldBe` count

getFirstFromDB dbFunction requestContext = do
  eitherList <- runInContextIO dbFunction requestContext
  liftIO $ isRight eitherList `shouldBe` True
  let (Right list) = eitherList
  return . head $ list

getOneFromDB dbFunction requestContext = do
  eitherOne <- runInContextIO dbFunction requestContext
  liftIO $ isRight eitherOne `shouldBe` True
  let (Right one) = eitherOne
  return one
