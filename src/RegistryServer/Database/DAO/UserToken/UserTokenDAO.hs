module RegistryServer.Database.DAO.UserToken.UserTokenDAO where

import qualified Data.UUID as U
import GHC.Int

import RegistryServer.Database.DAO.Common
import RegistryServer.Database.Mapping.UserToken.UserToken ()
import RegistryServer.Model.Context.ContextLenses ()
import RegistryServer.Model.Context.RequestContext
import RegistryServer.Model.UserToken.UserToken
import Shared.Model.Common.Sort

entityName = "user_token"

findUserTokensByUserUuidAndType :: U.UUID -> UserTokenType -> RequestContextM [UserToken]
findUserTokensByUserUuidAndType userUuid tokenType =
  createFindEntitiesBySortedFn entityName [("user_uuid", U.toString userUuid), ("type", show tokenType)] [Sort "createdAt" Ascending]

findUserTokenByUuidAndUserUuid :: U.UUID -> U.UUID -> RequestContextM UserToken
findUserTokenByUuidAndUserUuid uuid userUuid =
  createFindEntityByFn entityName [("uuid", U.toString uuid), ("user_uuid", U.toString userUuid)]

insertUserToken :: UserToken -> RequestContextM Int64
insertUserToken = createInsertFn entityName

deleteUserTokens :: RequestContextM Int64
deleteUserTokens = createDeleteEntitiesFn entityName

deleteUserTokenByUuid :: U.UUID -> RequestContextM Int64
deleteUserTokenByUuid uuid = createDeleteEntityByFn entityName [("uuid", U.toString uuid)]
