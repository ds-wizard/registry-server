module RegistryServer.Service.UserToken.UserTokenService where

import Control.Monad (void)
import Control.Monad.Reader (liftIO)
import Data.Time
import qualified Data.UUID as U

import RegistryServer.Api.Resource.UserToken.UserTokenDTO
import RegistryServer.Database.DAO.Common
import RegistryServer.Database.DAO.UserToken.UserTokenDAO
import RegistryServer.Model.Context.RequestContext
import RegistryServer.Model.Context.RequestContextHelpers
import RegistryServer.Model.User.User
import RegistryServer.Model.UserToken.UserToken
import RegistryServer.Model.UserToken.UserTokenList
import RegistryServer.Service.UserToken.UserTokenMapper
import Shared.Util.Crypto (generateRandomString)
import Shared.Util.Uuid

getTokens :: UserTokenType -> RequestContextM [UserTokenList]
getTokens tokenType = do
  user <- getCurrentUser
  fmap toList <$> findUserTokensByUserUuidAndType user.uuid tokenType

createToken :: U.UUID -> String -> UserTokenType -> Maybe UTCTime -> RequestContextM UserTokenDTO
createToken userUuid name tokenType expiresAt = do
  uuid <- liftIO generateUuid
  value <- liftIO $ generateRandomString 48
  now <- liftIO getCurrentTime
  let userToken = toUserToken uuid name tokenType userUuid value expiresAt now
  insertUserToken userToken
  return $ toDTO value userToken

deleteTokenByUuid :: U.UUID -> RequestContextM ()
deleteTokenByUuid uuid =
  runInTransaction $ do
    user <- getCurrentUser
    _ <- findUserTokenByUuidAndUserUuid uuid user.uuid
    void $ deleteUserTokenByUuid uuid
