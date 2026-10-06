module Specs.Api.Handler.Common where

import Data.Aeson (Key, Object, Value (..), encode)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.CaseInsensitive as CI
import Data.Either (isRight)
import qualified Data.List as L
import qualified Data.Text as T
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

boundary :: String
boundary = "X-TEST-BOUNDARY"

reqCtMultipartHeader :: Header
reqCtMultipartHeader = ("Content-Type", BS.pack $ "multipart/form-data; boundary=" ++ boundary)

createMultipartBody :: String -> String -> BSL.ByteString -> BSL.ByteString
createMultipartBody fileName contentType content =
  BSL.concat
    [ BSL.pack $ "--" ++ boundary ++ "\r\n"
    , BSL.pack $ "Content-Disposition: form-data; name=\"file\"; filename=\"" ++ fileName ++ "\"\r\n"
    , BSL.pack $ "Content-Type: " ++ contentType ++ "\r\n\r\n"
    , content
    , BSL.pack "\r\n"
    , BSL.pack $ "--" ++ boundary ++ "--\r\n"
    ]

adjustKey :: Key -> (Value -> Value) -> Object -> Object
adjustKey key f = KM.mapWithKey (\k v -> if k == key then f v else v)

toLegacyIdFields :: Key -> Key -> Object -> Object
toLegacyIdFields organizationKey entityKey o =
  case KM.lookup "id" o of
    Just (String id) ->
      let (organizationId, entityId) = T.breakOnEnd "." id
       in KM.insert organizationKey (String (T.dropEnd 1 organizationId)) . KM.insert entityKey (String entityId) . KM.delete "id" $ o
    _ -> o

toLegacyReferenceField :: Key -> Object -> Object
toLegacyReferenceField prefix o =
  case (KM.lookup idKey o, KM.lookup versionKey o) of
    (Just (String id), Just (String version)) ->
      let (organizationId, entityId) = T.breakOnEnd "." id
       in KM.insert idKey (String (T.intercalate ":" [T.dropEnd 1 organizationId, entityId, version])) . KM.delete versionKey $ o
    _ -> o
  where
    idKey = prefix <> "Id"
    versionKey = prefix <> "Version"

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
