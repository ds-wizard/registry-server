module Specs.Api.Handler.ApiKey.List_POST (
  list_POST,
) where

import Data.Aeson (eitherDecode, encode)
import qualified Data.ByteString.Char8 as BS
import Network.HTTP.Types
import Network.Wai (Application)
import Network.Wai.Test hiding (request)
import Test.Hspec
import Test.Hspec.Wai

import RegistryServer.Api.Resource.UserToken.ApiKeyCreateDTO
import RegistryServer.Api.Resource.UserToken.ApiKeyCreateJM ()
import RegistryServer.Api.Resource.UserToken.UserTokenDTO
import RegistryServer.Api.Resource.UserToken.UserTokenJM ()
import RegistryServer.Database.DAO.UserToken.UserTokenDAO
import RegistryServer.Database.Migration.Development.User.Data.Users
import RegistryServer.Database.Migration.Development.UserToken.Data.UserTokens
import RegistryServer.Model.Context.RequestContext
import RegistryServer.Model.User.User
import RegistryServer.Model.UserToken.UserToken
import Shared.Util.Crypto (hashSHA256)

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common

-- ------------------------------------------------------------------------
-- POST /api/api-keys
-- ------------------------------------------------------------------------
list_POST :: RequestContext -> SpecWith ((), Application)
list_POST requestContext =
  describe "POST /api/api-keys" $ do
    test_201 requestContext
    test_400 requestContext
    test_401 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/api-keys"

reqHeaders = [reqUserAuthHeader, reqCtHeader]

reqDto = apiKeyCreateDto

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_201 requestContext =
  it "HTTP 201 CREATED" $ do
    -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- THEN: Compare response with expectation
    let (SResponse (Status status _) _ body) = response
    liftIO $ status `shouldBe` 201
    let (Right resDto) = eitherDecode body :: Either String UserTokenDTO
    liftIO $ resDto.expiresAt `shouldBe` Nothing
    -- AND: Only the hash of the value is stored
    apiKeys <- getOneFromDB (findUserTokensByUserUuidAndType userNikola.uuid ApiKeyUserTokenType) requestContext
    let newApiKeys = filter (\k -> k.name == reqDto.name) apiKeys
    liftIO $ fmap (.valueHash) newApiKeys `shouldBe` [hashSHA256 resDto.token]
    -- AND: The value authenticates
    let authHeader = ("Authorization", BS.pack $ "Bearer " ++ resDto.token)
    currentResponse <- request methodGet "/api/users/current" [authHeader] ""
    let (SResponse (Status currentStatus _) _ _) = currentResponse
    liftIO $ currentStatus `shouldBe` 200

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 requestContext = createInvalidJsonTest reqMethod reqUrl "name"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody
