module RegistryServer.Database.DAO.User.UserDAO where

import Data.String
import Data.Time
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import RegistryServer.Database.DAO.Common
import RegistryServer.Database.Mapping.User.User ()
import RegistryServer.Model.Context.ContextLenses ()
import RegistryServer.Model.Context.RequestContext
import RegistryServer.Model.User.User
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort

entityName = "user_entity"

pageLabel = "users"

findUsers :: RequestContextM [User]
findUsers = createFindEntitiesSortedFn entityName [Sort "email" Ascending]

findUsersPage :: Maybe String -> Maybe String -> Pageable -> [Sort] -> RequestContextM (Page User)
findUsersPage mQuery mRole pageable sort =
  createFindEntitiesPageableQuerySortFn
    entityName
    pageLabel
    pageable
    sort
    "*"
    "WHERE (concat(first_name, ' ', last_name) ~* ? OR email ~* ?) AND role ~* ?"
    [regexM mQuery, regexM mQuery, regexM mRole]

findUserByUuid :: U.UUID -> RequestContextM User
findUserByUuid uuid = createFindEntityByFn entityName [("uuid", U.toString uuid)]

findUserByEmail :: String -> RequestContextM User
findUserByEmail email = createFindEntityByFn entityName [("email", email)]

findUserByEmail' :: String -> RequestContextM (Maybe User)
findUserByEmail' email = createFindEntityByFn' entityName [("email", email)]

findUserByTokenHash' :: String -> RequestContextM (Maybe User)
findUserByTokenHash' valueHash = do
  let sql =
        fromString
          "SELECT u.* \
          \FROM user_entity u \
          \JOIN user_token t ON t.user_uuid = u.uuid \
          \WHERE t.value_hash = ? AND (t.expires_at IS NULL OR t.expires_at > now())"
  let params = [valueHash]
  logQuery sql params
  let action conn = query conn sql params
  entities <- runDB action
  case entities of
    [entity] -> return . Just $ entity
    _ -> return Nothing

insertUser :: User -> RequestContextM Int64
insertUser = createInsertFn entityName

updateUserByUuid :: User -> RequestContextM Int64
updateUserByUuid user = do
  let sql =
        fromString
          "UPDATE user_entity SET uuid = ?, email = ?, first_name = ?, last_name = ?, password_hash = ?, role = ?, active = ?, created_at = ?, updated_at = ? WHERE uuid = ?"
  let params = toRow user ++ [toField user.uuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

updateUserPasswordByUuid :: U.UUID -> String -> UTCTime -> RequestContextM Int64
updateUserPasswordByUuid uuid passwordHash updatedAt = do
  let sql = fromString "UPDATE user_entity SET password_hash = ?, updated_at = ? WHERE uuid = ?"
  let params = [toField passwordHash, toField updatedAt, toField uuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteUsers :: RequestContextM Int64
deleteUsers = createDeleteEntitiesFn entityName

deleteUserByUuid :: U.UUID -> RequestContextM Int64
deleteUserByUuid uuid = createDeleteEntityByFn entityName [("uuid", U.toString uuid)]
